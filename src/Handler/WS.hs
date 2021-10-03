{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Handler.WS where

import Import

import Control.Concurrent
import Control.Lens hiding ((.=))
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
--import Control.Concurrent.STM
--import Control.Concurrent.STM.Lifted
import Data.Aeson
import Data.Aeson.Text
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Function (fix)
import Data.Text as Text hiding (empty, foldr)
import Data.Text.Lazy as TextL (toStrict)
import qualified Network.WebSockets as Net
import UnliftIO.Async
import Yesod.WebSockets


import Postfoundation
import Prefoundation

import Streamer
import UnliftIO (readTVarIO)
import UnliftIO.STM (writeTVar)
import Internal.Handy.NoFoundation (getCurrentTime)

getWSR :: Handler ()
getWSR = webSockets ws

{-
there are 3 stages to ws
1: set up user connection
2: main loop
3: disconnection clean up
-}
ws :: WebSocketsT Handler ()
ws = do
  App {..} <- lift getYesod
  maybeCurrentUser <- lift getCurrentUser
  -- STAGE 1: setup user connection and tqueue for this ws connection
  let defaultGESub = GESub False False False -- NOTE 1
  myConnId <- getCurrentTime
  (myTQGlobal, permissions, maybeLoggedIn) <- atomically $ do
    -- myTQGlobal receives all permited global events
    -- i.e. main chat messages, stream status updates, shout out box notifications
    -- permitted events are determined with GESub
    myTQGlobal <- newTQueue
    modifyTVar' tvWSConns $ HashMap.insert myConnId (defaultGESub, myTQGlobal)
    -- if the user is logged in, maybetqNotifications receives personal notifications
    -- i.e. whispers, personal user updates
    (permissions, maybeLoggedIn) <- case maybeCurrentUser of
      Nothing -> return (0, Nothing)
      Just (tvCurrentUser, currentUser@User{..}) -> do
        --modifyTVar' tvUsersInChat $ HashMap.insert _userId currentUser
        writeTQueue tqGlobalEvents $ MainChatGlobalEvent $ AddUser currentUser
        tqNotifications <- newTQueue
        userConns <- readTVar tvUserConns
        let newUserConns = (\f -> HashMap.alter f _userId userConns) $ Just . pair tvCurrentUser . \case
              Just (_, multiconns) -> HashMap.insert myConnId tqNotifications multiconns
              _ -> HashMap.singleton myConnId tqNotifications
        writeTVar tvUserConns newUserConns
        permissions <- getRolePower _role
        return (permissions, Just (tvCurrentUser, tqNotifications))
    return (myTQGlobal, permissions, maybeLoggedIn)
  wsState@ WSState {..} <- WSState myConnId permissions <$> newTVarIO myConnId

  -- STAGE 2: Main Loop
  -- do all actions forever until an exception
  raceMany $ foreverE <$> catMaybes
    [Just $ sendFromTQueue permissions myTQGlobal -- send all data that comes into my custom tqueue
    ,Just $ pingPong wsState
    ,Just $ receiveDataE >>= \case
      Left _ -> stop
      Right str -> processWSRequest wsState maybeLoggedIn str -- process all requests
      -- notifications require user to be logged in
    ,maybeLoggedIn <&> \loggedIn@(_, tqNotifications) ->
        atomically (readTQueue tqNotifications) >>= processNotifications wsState loggedIn
    ]

  -- STAGE 3: when an exception occurs, disconnect
  atomically $ do
    modifyTVar' tvWSConns $ HashMap.delete myConnId
    maybeCurrentUser `for_` \(tvCurrentUser, User{..}) -> do
      modifyTVar' tvUserConns $ flip HashMap.update _userId $ \(user, conns) ->
        let newConns = HashMap.delete myConnId conns
        in if HashMap.null newConns then Nothing else Just (user, newConns)
      --modifyTVar' tvUsersInChat $ HashMap.delete _userId
      writeTQueue tqGlobalEvents $ MainChatGlobalEvent $ RemoveUser _username




pingPong :: WSState -> WSResult
pingPong WSState{..} = do
  liftIO $ threadDelay $ 4 * minutes - 10 * seconds
  sendPingE ("PING" :: Text) >>= \case
    Left _ -> stop
    Right _ -> do
      liftIO $ threadDelay $ 10 * seconds
      currentTime <- getCurrentTime
      timeAtLastPong <- readTVarIO tvTimeAtLastPong
      if 10 * seconds > timeAtLastPong - currentTime
         then do sendClose ("Close" :: Text)
                 stop
         else continue






processWSRequest :: WSState -> Maybe LoggedIn -> BS.ByteString -> WSResult
processWSRequest WSState{..} _ "PONG" = do
  currentTime <- getCurrentTime
  atomically $ writeTVar tvTimeAtLastPong currentTime
  continue
processWSRequest WSState{..} maybeLoggedIn str = flip (maybe continue) (decode str) $ \wsRequest -> do
  App {..} <- lift getYesod
  case wsRequest of
    GESubscription geSub@GESub {..} -> do
      atomically $ modifyTVar' tvWSConns $ flip HashMap.adjust myConnId $ set _1
        geSub {recieveModEvents = recieveModEvents && permissions > 0
              --,recieveSubDono = recieveSubDono && Just True == permissions
              }
      continue
    --GetStreamStatus -> sendValueData . toJSON . StreamStatusGlobalEvent =<<
    --  liftIO (readTVarIO tvStreamStatus)
    --GetUsersInChat -> do
    --  usersInChat <- readTVarIO tvUserConns >$>= readTVarIO . fst
    --  sendValueData <=< jsonResponse "main_chat" $ -- send list users in main chat
    --    chatMessageJson $ SetUserList $ HashMap.fromList $
    --      (pair =<< _username) <$> HashMap.elems usersInChat
    _ -> continue



processNotifications :: WSState -> LoggedIn -> Notification -> WSResult
processNotifications WSState {..} (tvCurrentUser, tqNotifications) noti = do
  App {..} <- lift getYesod
  case noti of
    _ -> continue







continue :: WSResult
continue = return True
stop :: WSResult
stop = return False


raceMany :: [WSHandler a] -> WSHandler a
raceMany = runConcurrently . foldr (\b a -> a <|> Concurrently b) empty



foreverE :: Monad m => m Bool -> m ()
foreverE m = do
  continue <- m
  when continue $ foreverE m

--receiveDataForeverE :: WSHandler [Either SomeException BS.ByteString]
--receiveDataForeverE = do
--  eitherData <- receiveDataE
--  (eitherData ::) =<< receiveDataForeverE

sendFromTQueue :: Int -> TQueue PValue -> WSResult
sendFromTQueue permissions tqueue = sendValueData =<<
  atomically (readTQueue tqueue ?? permissions)

sendValueData :: Value -> WSResult
sendValueData = fmap exceptionToResult . sendTextDataE . TextL.toStrict . encodeToLazyText


exceptionToResult :: Either SomeException () -> Bool
exceptionToResult = isRight



type WSResult = WSHandler Bool

type WSHandler = WebSocketsT Handler

type LoggedIn = (TVar User, TQueue Notification)

data WSState = WSState
  {myConnId :: Int
  ,permissions :: Int
  ,tvTimeAtLastPong :: TVar Int
  }




{-
NOTES

1 Default Global Events Subscription: 
  - All GE subscriptions are set to False by default
  - Users can change their subscriptions via a request using Web Sockets
  - The alternative would be to assume the user will send a GESub request and wait for it to come
  - This halts the rest of the thread for no good reason because the user can change it later
  - We just view the first GESub request as a "change" rather than an initial state

2

-}