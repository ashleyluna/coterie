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

module Handler.Api.Mod where

import Control.Monad.Trans.Maybe
import Control.Lens hiding ((.=))
import Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as P
import qualified Data.Text as Text

import Import
import qualified Model as DB

import Internal.Api
import Internal.LiveInfo
import Internal.Payment
import Internal.StreamerInfo
import Internal.User
import Internal.WSRR

postModR :: Handler Value
postModR = do
  modRequest <- (requireCheckJsonBody :: Handler ModRequest)
  apiIfLoggedIn $ \User {..} -> do
    App {..} <- getYesod
    case modRequest of
      ChangeStreamStatus changeReq -> do
        let changeStreamStatus status = do
              writeTVar tvStreamStatus status
              writeTQueue tqGlobalEvents $ StreamStatus status
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




data ModRequest
  = ChangeStreamStatus ChangeStreamStatus
  deriving (Show)

data ChangeStreamStatus
  = ChangeTitle {title :: Text}
  | Host {creator :: Text}
  | GoOffline
  deriving (Show)


instance FromJSON ModRequest where
  parseJSON = withObject "ModRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "change_title" -> ChangeStreamStatus <$> ChangeTitle <$> obj .: "title"
      "host" -> ChangeStreamStatus <$> Host <$> obj .: "creator"
      "go_offline" -> return $ ChangeStreamStatus GoOffline
      str -> fail $ "Do Not Recognize Mod Request Type: " ++ str
