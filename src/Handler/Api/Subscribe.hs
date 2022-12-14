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

module Handler.Api.Subscribe
  (postGiftSubCheckR
  ,postMySubscribeR
  ) where

import Control.Monad.Fail
import Data.Aeson
import Data.Char as Char
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX as POSIX
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql as P
import System.Random as SyR

import Import
import qualified Model as DB

import Postfoundation.Api
import Internal.StreamerInfo
import Prefoundation




-- check if a user can be given a gift subscription
postGiftSubCheckR :: Handler Value
postGiftSubCheckR = do
  CheckUsername checkUsername <- requireCheckJsonBody
  checkResult <- findUser checkUsername >>= \case
    -- user does not exist
    Nothing -> return 2
    Just (_, User{..}) -> do
      maybeBool <- maybeAuthId <&&> (==) _userId . P.fromSqlKey
      if maybeBool == Just True then return 3 -- User Cannout Gift Themselves
         else case _role of
           Right _ -> return 4 -- user cannot be gifted a subscription
           Left (Chatter _ (Just _)) -> return 5 -- user is already subscribed
           _ -> return 1 -- user can be gifted a subscription
  -- 1 == User Can Be Gifted A Subscription
  -- 2 == User Does Not Exist
  -- 3 == User Cannout Gift Themselves
  -- 4 == User Cannot Be Given A Subscription
  -- 5 == User Is Already Subscribed
  jsonResponse "check_username"
    ["username" .= (checkResult :: Int)]


data CheckRequest
  = CheckUsername
    {checkUsername :: Text}
  deriving (Show)

instance FromJSON CheckRequest where
  parseJSON = withObject "CheckRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "check_username" ->
        CheckUsername <$> obj .: "username"
      str -> fail $ "Do Not Recognize GiftFriend Request Type: " ++ str


--------------------------------------------------------------------------------



postMySubscribeR :: Handler Value
postMySubscribeR = do
  requireCheckJsonBody >>= \case 
    SubscribeSelf{..} -> apiIfLoggedIn $ \tvUser User{..} ->
      apiIfNoProfanity message $ apiIfUserNotRole _role $ \_ -> do
        App {..} <- getYesod
        let amount = fromIntegral (tierCost tier) * if threeMonthPackage then 2.4 else 1
            userId = toSqlKey _userId
        -- TODO payment
        let currency = "USD"
        -- assume sucess
        paymentId <- runDB $ P.insert $
          DB.Payment (Just userId) amount currency
        subId <- runDB $ P.insert $
          DB.Sub paymentId userId Nothing tier threeMonthPackage message "Paypal"
        jsonError "TODO"
    SubscribeFriend{..} -> apiIfUserExists friendName $ \(tvUser, User{..}) ->
      apiIfNoProfanity message $ apiIfUserNotRole _role $ \Chatter {..} -> apiIfUserNotSubbed _subscription $
        -- TODO payment
        -- assume sucess
        jsonError "TODO"
    SubscribeRandom{..} ->
      let amount = round $ fromIntegral (tierCost tier * numOfGifts)
                         * if threeMonthPackage then 2.4 else 1
      in if numOfGifts < 1 then jsonError "Invalid Number Of Gift Subsubscriptions"
      else if amount > 500 then jsonError "Invalid Amount"
      else do
        -- TODO payment
        -- assume sucess
        jsonError "TODO"

    Donation {..} ->
      if not $ between 1 500 amount
      then jsonError "Invalid Amount"
      else apiIfNoProfanity message $ do
        -- TODO payment
        -- assume sucess
        jsonResponse "donation"
          []

{-
donation (user ambiguous)

sub self (user req)
sub friend (user ambiguous)
sub random (user ambiguous)
-}


data SubscribeRequest
  = SubscribeSelf
      {tier :: Int
      ,threeMonthPackage :: Bool
      ,message :: Text
      ,autoRenew :: Bool}
  | SubscribeFriend
      {tier :: Int
      ,threeMonthPackage :: Bool
      ,message :: Text
      ,friendName :: Text}
  | SubscribeRandom
      {tier :: Int
      ,threeMonthPackage :: Bool
      ,message :: Text
      ,numOfGifts :: Int}
  | Donation
      {donorName :: Text
      ,amount :: Double
      ,message :: Text}


instance FromJSON SubscribeRequest where
  parseJSON = withObject "SubscribeRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "subscribe_self" ->
        SubscribeSelf <$> obj .: "tier"
                      <*> obj .: "3_month_package"
                      <*> obj .: "message"
                      <*> obj .: "auto_renew"
      "subscribe_friend" ->
        SubscribeFriend <$> obj .: "tier"
                        <*> obj .: "3_month_package"
                        <*> obj .: "message"
                        <*> obj .: "username"
      "subscribe_random" ->
        SubscribeRandom <$> obj .: "tier"
                        <*> obj .: "3_month_package"
                        <*> obj .: "message"
                        <*> obj .: "num_of_gifts"
      "donation" ->
        Donation <$> obj .: "name"
                 <*> obj .: "amount"
                 <*> obj .: "message"
      str -> fail $ "Do Not Recognize Subscribe Request Type: " ++ str



--------------------------------------------------------------------------------

tierCost tier = 5 * (2 ^ (tier - 1))

subCost tier =
  (tierCost tier , round $ fromIntegral (tierCost tier) * 2.4)


apiIfUserExists username f = findUser username >>= \case
  Nothing -> jsonError "User Doesn't Exist"
  Just user -> f user

apiIfUserNotSubbed sub m = case sub of
  Just _ -> jsonError "User Is Already Subscribed"
  _ -> m

apiIfUserNotRole :: Either t b -> (t -> Handler Value) -> Handler Value
apiIfUserNotRole role f = case role of
  Right _ -> jsonError "User Cannot Be Given A Subscription"
  Left chatter -> f chatter

apiIfDBUserNotRole maybeRole m = if isJust maybeRole
  then jsonError "User Cannot Be Given A Subscription"
  else m
