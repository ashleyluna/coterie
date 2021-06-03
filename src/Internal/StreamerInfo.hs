{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.StreamerInfo where

import ClassyPrelude.Yesod

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Text


-- this information should be static/very rarely change
-- since changes should be very rare, they will be manual
data StreamerInfo = StreamerInfo
  {rootUrl :: Text
  ,streamerName :: Text -- user account name, DB.User username
  ,seasonEndTimes :: [Word] -- Epoch dates of season beginnings, in seconds
                            -- first date is the end of season 1 / beginning of season 2
  }


data Secrets = Secrets
  {googleClientId :: Text
  ,googleClientSecret :: Text
  ,twitchClientId :: Text
  ,twitchClientSecret :: Text
  ,twitchStreamerId :: Text
  ,paypalSandbox :: Text
  ,paypalClientId :: Text
  ,paypalClientSecret :: Text
  }
