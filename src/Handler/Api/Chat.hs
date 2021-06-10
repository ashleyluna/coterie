{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Api.Chat where

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

import Internal.Api
import Internal.WS
import Internal.User

import Streamer


postChatR :: Handler Value
postChatR = apiIfLoggedIn $ \currentUser@User {..} -> do
  App {..} <- getYesod
  timeStamp <- currentTime
  let sendMessage tvar message sendEvent = do
        modifyTVar' tvar $ IntMap.insert timeStamp message
        writeTQueue tqGlobalEvents $ sendEvent message
  requireCheckJsonBody >>= \case
    MainChatMessageRequest message -> do
      let userMessage = UserMessage _userId timeStamp message False
          sendToMainChat = do
            sendMessage tvMainChat userMessage $ MainChat . AddUserMessage currentUser
          delayMessage = do
            atomically $ sendMessage tvMainChat userMessage $ ModEvent . AddDelayedUserMessage currentUser
            liftIO $ threadDelay $ 2 * seconds
            atomically $ do
              mainChatDelayed <- readTVar tvMainChatDelayed
              when (IntMap.member timeStamp mainChatDelayed) $ do
                modifyTVar' tvMainChatDelayed $ IntMap.delete timeStamp
                sendToMainChat
      if isMod _role || isSafeUser _moderation
        then atomically sendToMainChat
        else delayMessage
      jsonResponse "message" []
    -- no need to moderate
    ModChatMessageRequest message -> do
      when (isMod _role) $ do
        timeStamp <- currentTime
        let userMessage = UserMessage _userId timeStamp message False
            addUserMessage = AddUserMessage currentUser userMessage
        timeStamp <- currentTime
        atomically $ writeTQueue tqGlobalEvents $ ModEvent $ ModChat addUserMessage
      jsonResponse "message" []
    -- WhisperMessageRequest
    _ -> jsonResponse "TODO" []
    --UserInfo -> jsonResponse "chat_message" []
    --ModerationRequest modRequest -> if not $ isMod _role then continue else case modRequest of
    --  ModAction _ -> jsonResponse "chat_message" []








data ChatRequest
  = MainChatMessageRequest
      {message :: Text}
  | ModChatMessageRequest
      {message :: Text}
  | WhisperMessageRequest
      {receiver :: Text
      ,message :: Text}
  | UserInfo
  | ModerationRequest ModerationRequest -- must be mod

data ModerationRequest
  = ModAction ModAction


data ModAction
  = Censor Text Int -- username time length
  | Ban Text -- username



instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \obj -> obj .: "type" >>= \case
    "message" -> obj .: "type_chat" >>= \case
      "main" -> MainChatMessageRequest <$> obj .: "message"
      "mod" -> ModChatMessageRequest <$> obj .: "message"
      "whisper" -> WhisperMessageRequest <$> obj .: "receiver"
                                         <*> obj .: "message"
      str -> fail $ "Do Not Recognize Chat Type Request Type: " ++ str
    str -> fail $ "Do Not Recognize Chat Request Type: " ++ str



--data ChatRequest
--  = Message
--      {chatLocale :: ChatLocale
--      ,message :: Text}
--
--data ChatLocale
--  = MainChatLocale
--  | ModChatLocale
--  | WhisperLocale {receiver :: Text}
--
--instance FromJSON ChatRequest where
--  parseJSON = withObject "ChatRequest" $ \obj -> do
--    let chatLocale = obj .: "chat_locale" >>= \case
--          "main" -> return MainChatLocale
--          "mod" -> return ModChatLocale
--          "whisper" -> WhisperLocale <$> obj .: "receiver"
--          str -> fail $ "Do Not Recognize Chat Locale Type: " ++ str
--    obj .: "type" >>= \case
--      "message" -> Message <$> chatLocale
--                           <*> obj .: "message"
--      str -> fail $ "Do Not Recognize Chat Request Type: " ++ str
