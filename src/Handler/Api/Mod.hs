{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handler.Api.Mod
 (postModR
 ) where

import Control.Monad.Trans.Maybe
import Control.Lens hiding ((.=))
import Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as P
import qualified Data.Text as Text

import Import
import qualified Model as DB

import Postfoundation
import Internal.StreamerInfo
import Prefoundation

postModR :: Handler Value
postModR = apiIfLoggedIn $ \tvCurrentUser User{..} -> do
  App {..} <- getYesod
  requireCheckJsonBody >>= \case
    ChangeStreamStatus changeReq -> do
      let changeStreamStatus status = do
            writeTVar tvStreamStatus status
            writeTQueue tqGlobalEvents $ StreamStatusGlobalEvent status
      case changeReq of
        ChangeTitle title -> do
          atomically $ do
            oldStreamStatus <- readTVar tvStreamStatus
            case oldStreamStatus of
              Streaming {..} -> do
                changeStreamStatus $ oldStreamStatus {_title = title}
              _ -> return ()
          jsonResponse "change_title" []
        Host creator -> jsonResponse "host" []
        GoOffline -> do
          atomically $ do
            changeStreamStatus Offline
          jsonResponse "go_offline" []
    RequestUserMessageHistory messageHistoryRequest -> do
      usersMessages <- fmap catMaybes $ runDB $ case messageHistoryRequest of
        InitialMessages usernames -> usernames `for` \username ->
          P.getBy (DB.UniqueUsername username) >>=- \(Entity userId _) -> do
            messages <- P.selectList
              [DB.UserMessageSender P.==. userId]
              [P.LimitTo 51
              ,P.Desc DB.UserMessageTimestamp]
            return (username, length messages < 51, take 50 messages)
        MiddleMessages past users -> users `for` \(username, timestamp) ->
          P.getBy (DB.UniqueUsername username) >>=- \(Entity userId _) -> do
            messages <- P.selectList
              [DB.UserMessageSender P.==. userId
              ,if past then DB.UserMessageTimestamp P.<. timestamp
                       else DB.UserMessageTimestamp P.>. timestamp]
              [P.LimitTo 51
              ,(if past then P.Desc else P.Asc) DB.UserMessageTimestamp]
            return (username, length messages < 51, take 50 messages)
      jsonResponse "user_messages"
        ["users" .= flip fmap usersMessages \(username, end, messages) -> object
          ["username" .= username
          ,"end" .= end
          ,"messages" .= flip fmap messages \(Entity _ message) -> object
            ["timestamp" .= DB.userMessageTimestamp message
            ,"message" .= DB.userMessageMessage message]]]
    _ -> jsonError ""
    --  (atTop, userMessages) <- runDB $ do
    --    messages <- E.select $ do
    --      (user E.:& message) <- E.from $
    --        E.Table @DB.User `E.InnerJoin` E.Table @DB.UserMessage `E.on`
    --          \(user E.:& message) -> user E.^. DB.UserUsername `E.in_` E.valList usernames E.&&.
    --            case pivot of
    --              Initial -> E.val True
    --              Before before -> E.val before E.>. message E.^. DB.UserMessageTimestamp
    --              UpTo upTo -> message E.^. DB.UserMessageTimestamp E.>. E.val upTo
    --      case pivot of
    --        UpTo _ -> return ()
    --        _ -> E.limit 51
    --      E.orderBy [E.desc $ message E.^. DB.UserMessageTimestamp]
    --      pure (user, message)
    --    pure $ pair (length messages < 51) $ Map.toList $
    --      foldr (\(Entity _ user, Entity _ message) -> Map.alter (Just . fromMaybe [message] . fmap (message:)) $ DB.userUsername user)
    --            Map.empty
    --            (take 50 messages)
    --  jsonResponse "user_messages" $
    --    ["users" .= object ¢ flip fmap userMessages ¢ \(username, messages) ->
    --      username .= flip fmap messages ¢ \(DB.UserMessage {..}) -> object
    --        ["time" .= userMessageTimestamp
    --        ,"message" .= userMessageMessage
    --        ]
    --    ,"at_top" .= atTop -- are there any more messages that can be requested
    --    ]



data ModRequest
  = ChangeStreamStatus ChangeStreamStatus
  | RequestUserMessageHistory MessageHistoryRequest
  | BanRequest
      {username :: Text}
  | MuteRequest
      {username :: Text
      ,muteLength :: Int} -- in minutes
  deriving (Show)

data ChangeStreamStatus
  = ChangeTitle {title :: Text}
  | Host {creator :: Text}
  | GoOffline
  deriving (Show)

data MessageHistoryRequest
  -- first fifty messages from each user
  = InitialMessages
      {usernames :: [Text]}
  -- fifty messages from each user, either before or after (not including) a certain time
  | MiddleMessages
      {past :: Bool
      ,users :: [(Text, Int)]}
  deriving (Show)

instance FromJSON ModRequest where
  parseJSON = withObject "ModRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "change_title" -> ChangeStreamStatus . ChangeTitle <$> (obj .: "title")
      "host" -> ChangeStreamStatus . Host <$> (obj .: "creator")
      "go_offline" -> return $ ChangeStreamStatus GoOffline
      "user_messages" -> RequestUserMessageHistory <$> asum
        [InitialMessages <$> obj .: "usernames"
        ,MiddleMessages <$> obj .: "past"
                        <*> (obj .: "users" >>=- \obj ->
                              pair <$> obj .: "username"
                                   <*> obj .: "timestamp")]
      str -> fail $ "Do Not Recognize Mod Request Type: " ++ str
