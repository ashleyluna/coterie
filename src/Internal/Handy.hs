{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Internal.Handy where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Time.Clock as Clock
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql as P
import System.Random as SyR

import Foundation
import Import.NoFoundation
import qualified Model as DB

import Prefoundation
import Data.ByteString.Char8 (putStr)


getCurrentUser :: Handler MaybeTVUser
--currentUser = maybeAuth >>=- fromUserDB
getCurrentUser = maybeAuthId >>== \userId -> do
  App{..} <- getYesod
  lookupUserId tvUsers $ P.fromSqlKey userId

findUser :: Text -> Handler MaybeTVUser
--findUser username = runDB (P.getBy $ DB.UniqueUsername username) >>=- fromUserDB_
findUser username = do
  App{..} <- getYesod
  runDB (P.getBy $ DB.UniqueUsername username) >>== \(Entity userId _) -> do
    lookupUserId tvUsers $ P.fromSqlKey userId
    
lookupUserId :: MonadIO m => TVHashMap Int64 TVUser -> UserId -> m MaybeTVUser
lookupUserId tvUsers userId = do
  HashMap.lookup userId <$> readTVarIO tvUsers >>=- \tvUser -> do
    user <- readTVarIO tvUser
    return (tvUser, user)

--lookupUserKey :: (ToBackendKey SqlBackend record, MonadIO m) =>
--  TVHashMap Int64 TVUser -> Key record -> m MaybeTVUser
--lookupUserKey tvUsers = lookupUserId tvUsers . P.fromSqlKey



modifyUser :: TVHashMap Int64 TVUser -> Int64
           -> [P.Update DB.User] -> (User -> User) -> Handler ()
modifyUser tvUser userId dbUpdates f = do
  runDB $ P.update (P.toSqlKey userId) dbUpdates
  atomically $ do
    maybeTVUser <- HashMap.lookup userId <$> readTVar tvUser
    forM_ maybeTVUser $ \tvUser -> modifyTVar' tvUser f

modifyUserConn :: TVHashMap Int64 TVUser
               -> Int64 -> (User -> User) -> STM ()
modifyUserConn tvUserConns userId f = do
  maybeTVUser <- HashMap.lookup userId <$> readTVar tvUserConns
  forM_ maybeTVUser $ \tvUser -> modifyTVar' tvUser f
-- modifyUser f = atomically $ modifyUserConn tvUserConns _userId f

type MaybeTVUser = Maybe (TVUser, User)

--------------------------------------------------------------------------------


randomDefaultColor :: MonadIO m => Int -> m DefaultColor
randomDefaultColor uniqueNumber = do -- uniqueNumber usually will == userCreationTime
  rand <- fst . SyR.random . SyR.mkStdGen
    . (+ uniqueNumber) . fromEnum . Clock.utctDay
    <$> liftIO Clock.getCurrentTime
  return $ case mod (rand :: Int) 17 of
    int | int <= 0 -> Blue
    1 -> Azure
    2 -> Sky
    3 -> Turquoise
    4 -> Turtle
    5 -> Green
    6 -> Slime
    7 -> Yellow
    8 -> Bronze
    9 -> Mocha
    10 -> Orange
    11 -> DarkRed
    12 -> Red
    13 -> Rose
    14 -> Pink
    15 -> Purple
    _ -> Lavender

