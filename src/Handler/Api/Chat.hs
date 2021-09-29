{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Handler.Api.Chat
 (postChatR
 ) where

import Control.Concurrent
import Control.Monad.Trans.Maybe
import Control.Lens hiding ((.=))
import Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashSet as HashSet
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as P
import qualified Data.Text as Text

import Import
import qualified Model as DB

import Postfoundation.Api
import Postfoundation.Moderation
import Prefoundation


import Streamer


postChatR :: Handler Value
postChatR = apiIfLoggedIn $ \tvCurrentUser currentUser@User {..} -> do
  App {..} <- getYesod
  currentTime <- currentTime
  let modifyUser = modifyUserConnDB tvUsers _userId
      dbUserId = P.toSqlKey _userId
      sendMessage tvar message sendEvent = do
        modifyTVar' tvar $ IntMap.insert currentTime message
        writeTQueue tqGlobalEvents $ sendEvent message
      limitChat tvchat = do
        chat <- readTVar tvchat
        when (IntMap.size chat > 500) $
          modifyTVar' tvchat IntMap.deleteMin
  requireCheckJsonBody >>= \case
    MainChatMessageRequest message -> do
      -- TODO log the number of times each emote get used
      let userMessage = UserMessage tvCurrentUser currentTime message False
          sendToMainChat = do
            sendMessage tvMainChat userMessage $
              MainChatGlobalEvent . AddUserMessage currentUser
            limitChat tvMainChat
          delayMessage = do
            atomically $ sendMessage tvMainChat userMessage $
              ModGlobalEvent . AddDelayedUserMessage currentUser
            liftIO $ threadDelay $ 2 * seconds
            atomically $ do
              mainChatDelayed <- readTVar tvMainChatDelayed
              if IntMap.member currentTime mainChatDelayed
                 then do modifyTVar' tvMainChatDelayed $ IntMap.delete currentTime
                         sendToMainChat
                         return True
                 else return False
      if False -- check profanity
         then jsonError "Invalid Language"
         else do -- check mute list
           maybeMutedTime <- IntMap.lookup (fromEnum _userId) <$> readTVarIO tvMuteList
           if fmap (< currentTime) maybeMutedTime == Just True
             then jsonError "Muted"
             else do -- allowed
               -- remove user from mute list if they were previously in it
               when (isJust maybeMutedTime) $ atomically $
                 modifyTVar' tvMuteList $ IntMap.delete (fromEnum _userId)
               -- add message to database
               runDB $ P.insert $ DB.UserMessage dbUserId currentTime message
               -- send message to mods/global
               wasMessageSent <- if isRight _role || isSafeUser _moderation
                 then do atomically sendToMainChat
                         return True
                 else delayMessage
               -- if message was not deleted by mods during delay, increment _meaningfulMessages
               if not wasMessageSent
                  then jsonError "Removed By Mods"
                  else do
                    streamStatus <- readTVarIO tvStreamStatus
                    isMessageMeaningful <- isMessageMeaningful message
                    when (isMessageMeaningful && isStreaming streamStatus) $
                      modifyUser
                        [DB.UserMeaningfulMessages =. _meaningfulMessages _moderation + 1]
                        $ over (moderation . meaningfulMessages) (+ 1)
                    jsonResponse "message" []
    -- no need to moderate
    ModChatMessageRequest message -> do
      atomically (isMod _role) >>= flip when do
        let userMessage = UserMessage tvCurrentUser currentTime message False
            addUserMessage = AddUserMessage currentUser userMessage
        -- add message to database
        runDB $ P.insert $ DB.UserModMessage dbUserId currentTime message
        atomically $ do
          sendMessage tvModChat userMessage $
            ModGlobalEvent . ModChat . AddUserMessage currentUser
          limitChat tvModChat
      jsonResponse "message" []
    -- WhisperMessageRequest
    --ModerationRequest modRequest -> if not $ isMod _role then continue else case modRequest of
    --  ModAction _ -> jsonResponse "chat_message" []
    UserInfo username -> findUser username >>= \case
      Nothing -> jsonError "User Not Found"
      Just (tvCurrentUser, User{..}) -> do
        modInfo <- do 
          isMod <- atomically $ isMod _role
          return $ if not $ isMod then []
          else ["meaningful_messages" .= _meaningfulMessages _moderation
               --,"mod_actions" .= ([] :: [Int])
               ]
        (permissions, toRoleJSON) <- atomically $ (,) <$> getRolePower _role <*> toPJSON_ _role
        jsonResponse "get_user_info" $
          ["role" .= toRoleJSON permissions
          ,"badges" .= toJSON
               (toList (_firstBadge _badges)
             ++ toList (_secondBadge _badges))
          ,"pronouns" .= _pronouns
          ,"name_color" .= nameColorJSON _role _nameColor
          ,"account_creation" .= _userCreationTime _accountInfo
          ,"power" .= permissions
          ,"season" .= _season _accountInfo
          ] ++ modInfo
    _ -> jsonResponse "TODO" []









data ChatRequest
  = MainChatMessageRequest
      {message :: Text}
  | ModChatMessageRequest
      {message :: Text}
  | UserInfo
      {usernameSearch :: Text}
--  | ModerationRequest ModerationRequest -- must be mod
--
--data ModerationRequest
--  = ModAction ModAction
--
--
--data ModAction
--  = Mute Text Int -- username time length
--  | Ban Text -- username



instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \obj -> obj .: "type" >>= \case
    "message" -> obj .: "type_chat" >>= \case
      "main" -> MainChatMessageRequest <$> obj .: "message"
      "mod" -> ModChatMessageRequest <$> obj .: "message"
      str -> fail $ "Do Not Recognize Chat Type Request Type: " ++ str
    "get_user_info" -> UserInfo <$> obj .: "username"
    str -> fail $ "Do Not Recognize Chat Request Type: " ++ str

{-
NOTES

Main Chat Messges
  - First check for profanity
  - Add message to DB (UserMessage Table)
  - If user is a special user or a "safe user", send to tvMainChat right away
    otherwise send message to mods adn tvMainChatDelayed, and delay message
    from tvMainChat for 2 seconds
  - If mods have not removed message from tvMainChatDelayed after 2 seconds,
    send to tvMainChat
  - If message is "meaningful", increment userMeaningfulMessages in DB (User
    Table) and in tvUserConns

-}
