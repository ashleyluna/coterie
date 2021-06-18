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
import Data.Maybe as Maybe
import Data.Text as Text hiding (empty, foldr)
import Data.Text.Lazy as TextL (toStrict)
import qualified Network.WebSockets as Net
import UnliftIO.Async
import Yesod.WebSockets


import Internal.Api
import Internal.WS
import Internal.User

import Streamer

getWSR :: Handler ()
getWSR = webSockets ws

{-
there are 4 stages to ws
1: set up user connection
2: main loop
3: disconnection clean up
-}
ws :: WebSocketsT Handler ()
ws = do
  App {..} <- lift getYesod
  currentUser <- lift currentUser
  -- STAGE 1: setup user connection
  let defaultGESub = GESub False False False
  myConnId <- currentTime
  (myTQGlobal, maybeUserConn) <- atomically $ do
    -- myTQGlobal receives all permited global events
    -- i.e. main chat messages, stream status updates, shout out box notifications
    -- permitted events are determined with GESub
    myTQGlobal <- newTQueue
    modifyTVar' tvWSConns $ HashMap.insert myConnId (defaultGESub, myTQGlobal)
    -- if the user is logged in, maybetqNotifications receives personal notifications
    -- i.e. whispers, personal user updates
    maybeUserConn <- currentUser `for` \currentUser@User {..} -> do
      --modifyTVar' tvUsersInChat $ HashMap.insert _userId currentUser
      writeTQueue tqGlobalEvents $ MainChat $ AddUser currentUser
      tqNotifications <- newTQueue
      userConns <- readTVar tvUserConns
      (tvCurrentUser, newUserConns) <- getCompose $ (\f -> HashMap.alterF f _userId userConns) $ Compose . \case
        Just currentUserConns@(tvCurrentUser, multiconns) -> do
          return $ pair tvCurrentUser $
            Just $ (tvCurrentUser, HashMap.insert myConnId tqNotifications multiconns)
        _ -> do
          tvCurrentUser <- newTVar currentUser
          return $ pair tvCurrentUser $
            Just (tvCurrentUser, HashMap.singleton myConnId tqNotifications)
      writeTVar tvUserConns newUserConns
      return (tvCurrentUser, tqNotifications)
    return (myTQGlobal, maybeUserConn)
  wsState@ WSState {..} <-
    WSState <$> return myConnId
            <*> newTVarIO defaultGESub
            <*> return maybeUserConn
            <*> (newTVarIO . flip subtract (2 * seconds) =<< currentTime)

  -- STAGE 2: Main Loop
  -- do all actions forever until an exception
  raceMany $ foreverE <$>
    [sendFromTQueue myTQGlobal -- send all data that comes into my custom tqueue
    ,do liftIO $ threadDelay $ 4 * minutes
        maybeException <$> sendPingE ("PING" :: Text)
    ,receiveDataE >>= \case
      Left e -> stopWith e
      Right str -> processWSRequest wsState str -- process all requests
    ] ++ -- notifications require user to be logged in
    case maybeLoggedIn of
      Nothing -> []
      Just loggedIn@(_, tqNotifications) ->
        [atomically (readTQueue tqNotifications) >>= processNotifications wsState loggedIn]

  -- STAGE 3: when an exception occurs, disconnect
  atomically $ do
    modifyTVar' tvWSConns $ HashMap.delete myConnId
    currentUser `for_` \User {..} -> do
      modifyTVar' tvUserConns $ flip HashMap.update _userId $ \(user, conns) ->
        let newConns = HashMap.delete myConnId conns
        in if HashMap.null newConns then Nothing else Just (user, newConns)
      --modifyTVar' tvUsersInChat $ HashMap.delete _userId
      writeTQueue tqGlobalEvents $ MainChat $ RemoveUser _username







processWSRequest :: WSState -> BS.ByteString -> WSException
processWSRequest _ "PONG" = continue
processWSRequest wsState@WSState {..} str = flip (maybe continue) (decode str) $ \wsRequest -> do
  App {..} <- lift getYesod
  case wsRequest of
    GESubscription geSub@GESub {..} -> do
      -- Nothing == Chatter, Just False == Mod, Just True == Admin
      putStrLn "TEST"
      putStrLn $ showText geSub
      putStrLn $ showText myConnId
      permissions <- maybeLoggedIn >>*= \(tvCurrentUser,_) -> do
          role <- _role <$> readTVarIO tvCurrentUser
          if | isAdmin role -> return $ Just True
             | isMod role   -> return $ Just False
             | otherwise    -> return Nothing
      atomically $ modifyTVar' tvWSConns $ flip HashMap.adjust myConnId $ set _1
        geSub {recieveModEvents = recieveModEvents && Nothing /= permissions
              --,recieveSubDono = recieveSubDono && Just True == permissions
              }
      continue
    GetStreamStatus -> sendValueDataR . toJSON . StreamStatus =<<
      liftIO (readTVarIO tvStreamStatus)
    GetUsersInChat -> do
      usersInChat <- readTVarIO tvUserConns >$>= readTVarIO . fst
      sendValueDataR <=< jsonResponse "main_chat" $ -- send list users in main chat
        chatMessageJson $ SetUserList $ HashMap.fromList $
          fmap (pair =<< _username) $ HashMap.elems usersInChat
    _ -> continue



processNotifications :: WSState -> (TVar User, TQueue Notification) -> Notification -> WSException
processNotifications wsState@WSState {..} (tvCurrentUser, tqNotifications) noti = do
  App {..} <- lift getYesod
  case noti of
    Whisper -> continue







continue :: WSException
continue = return Nothing
stopWith :: SomeException -> WSException
stopWith = return . Just


raceMany :: [WSHandler a] -> WSHandler a
raceMany = runConcurrently . foldr (\b a -> a <|> Concurrently b) empty



foreverE :: Monad m => m (Maybe a) -> m a
foreverE m = do
  m >>= \case
    Nothing -> foreverE m
    Just e -> return e

--receiveDataForeverE :: WSHandler [Either SomeException BS.ByteString]
--receiveDataForeverE = do
--  eitherData <- receiveDataE
--  (eitherData ::) =<< receiveDataForeverE

sendFromTQueue :: ToJSON a => TQueue a -> WSException
sendFromTQueue tqueue = fmap maybeException $
  sendValueData . toJSON =<< atomically (readTQueue tqueue)

sendValueDataR :: Value -> WSException
sendValueDataR = fmap exceptionToResult . sendValueData
sendValueData :: Value -> WSHandler (Either SomeException ())
sendValueData = sendTextDataE . TextL.toStrict . encodeToLazyText


exceptionToResult :: Either SomeException () -> Maybe SomeException
exceptionToResult (Right _) = Nothing
exceptionToResult (Left e) = Just e

maybeException :: Either SomeException () -> Maybe SomeException
maybeException (Right _) = Nothing
maybeException (Left e) = Just e



type WSException = WSHandler (Maybe SomeException)

type WSHandler = WebSocketsT Handler

data WSState = WSState
  {myConnId :: Int
  ,tvGESub :: TVar GESub
  ,maybeLoggedIn :: Maybe (TVar User, TQueue Notification)
  ,tvTimeSinceLastMessage :: TVar Int
  }
