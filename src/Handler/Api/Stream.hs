{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handler.Api.Stream
 (postStreamR
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

import Postfoundation
import Prefoundation


import Streamer
import ClassyPrelude.Yesod (readTVarIO)


postStreamR :: Handler Value
postStreamR = apiIfLoggedIn $ \tvCurrentUser currentUser@User{..} -> do
  App{..} <- getYesod
  currentTime <- currentTime
  let permissions = getRolePower _role
      modifyUser = modifyUserConnDB tvUsers _userId
      --dbUserId = P.toSqlKey _userId
      sendMessage tvar message sendEvent = do
        modifyTVar' tvar $ IntMap.insert currentTime message
        writeTQueue tqGlobalEvents $ sendEvent message
      limitChat tvchat = do
        chat <- readTVar tvchat
        when (IntMap.size chat > 150) $
          modifyTVar' tvchat IntMap.deleteMin
  requireCheckJsonBody >>= \case
    GetStreamStatus -> do
      streamStatus <- readTVarIO tvStreamStatus >>= atomically . toPJSON_ 
      jsonResponse "get_stream_status" $
        streamStatus 1
    --GetUsersInChat -> do
    --  usersInChat <- readTVarIO tvUserConns >$>= readTVarIO . fst
    --  jsonResponse "main_chat" $ -- send list users in main chat
    --    chatMessageToJson permissions $ SetUserList $ HashMap.fromList $
    --      (pair =<< _username) <$> HashMap.elems usersInChat
    --GetMainChat -> do 
    --  mainChat <- readTVarIO tvMainChat
    --  jsonResponse "get_main_chat" $
    --    mainChat
    _ -> jsonResponse "TODO" []









data StreamRequest
  = GetStreamStatus
  | GetUsersInChat
  | GetMainChat
  | GetSubscriptionNumbers



instance FromJSON StreamRequest where
  parseJSON = withObject "StreamRequest" $ \obj -> obj .: "type" >>= \case
    "get_stream_status" -> return GetStreamStatus
    "get_users_in_main_chat" -> return GetUsersInChat
    "get_main_chat" -> return GetMainChat
    "get_subscription_numbers" -> return GetSubscriptionNumbers
    str -> fail $ "Do Not Recognize Stream Request Type: " ++ str




{-
NOTES

-}
