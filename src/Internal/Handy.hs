{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.Handy where

import Data.Time.Clock as Clock
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql as P
import System.Random as SyR

import Foundation
import Import.NoFoundation
import qualified Model as DB

import Internal.User

--getUser :: Text -> Handler (Maybe (Entity DB.User))
--getUser = runDB . getBy . DB.UniqueUsername
--
--getSub = runDB . getBy . DB.UniqueSubber

randomDefaultColor :: Int -> Handler DefaultColor
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
