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

module Handler.Api.Profile where

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

import Postfoundation
import Internal.StreamerInfo
import Prefoundation


getMyProfileR :: Handler Value
getMyProfileR = apiIfLoggedIn $ \tvCurrentUser User{..} -> do
  App{..} <- getYesod
  let AccountInfo {..} = _accountInfo
  --role <- case _role of
  --  Right SpecialRole{..} -> return $ object
  --    ["name" .= _roleName
  --    ,"power" .= _power]
  --  Left Chatter{..} -> do
  --    maybeSubscription <- case _subscription of
  --      Nothing -> return Null
  --      Just Subscription{..} -> do
  --        maybeGifterName <- _maybeGifterId ->== lookupUserId tvUsers <&&>
  --          \(_, User{..}) -> _userId
  --        return $ object
  --          ["tier" .= _subTier
  --          ,"gifter" .= maybeGifterName
  --          ,"is_3_month_package" .= _is3MonthPackage
  --          ,"recurring" .= _recurring
  --          ,"end_time" .= _endTime
  --          ]
  --    return $ object
  --      ["months" .= _months
  --      ,"subscription" .= maybeSubscription
  --      ,"can_post_links" .= (isRight _role || isSafeUser _moderation)
  --      ]
  roleJSON <- atomically $ toPJSON_' _role
  jsonResponse "get_profile"
    ["account" .= object
      ["email" .= _userEmail
      ,"num_months_subbed" .= _numMonthsSubbed
      ,"season" .= _season
      ,"twitch_conn" .= isJust _twitchConn
      ,"google_conn" .= False
      ,"twitter_conn" .= False
      ,"reddit_conn" .= False
      ,"discord_conn" .= False
      ]
    ,"username" .= _username
    ,"role" .= object (roleJSON ++
      ["can_post_links" .= isSafeUser _moderation])
    ,"badges" .= _badges
    ,"pronouns" .= _pronouns
    ,"name_color" .= object
      ["default_name_color" .= toJSON (case _defaultNameColor _nameColor of
        Left color -> show color
        Right color -> show color)
      ,"is_random" .= toJSON (isLeft $ _defaultNameColor _nameColor)
      ,"left" .= _left _nameColor
      ,"right" .= _right _nameColor
      ,"mode" .= show (_mode _nameColor)
      ]
    -- points
    ]








postMyProfileR :: Handler Value
postMyProfileR = do
  App {..} <- getYesod
  profileRequest <- (requireCheckJsonBody :: Handler ProfileRequest)
  apiIfLoggedIn $ \tvCurrentUser User{..} -> do
    let StreamerInfo {..} = streamerInfo
        modifyCurrentUser = modifyUser tvUsers _userId
          --writeTVar tvUserConns =<< flip2 HashMap.alterF _userId userConns ¢
          --  traverse ¢ \(user, threads) -> do
          --    traverse (flip writeTQueue $ SelfUpdate $ f user) $ HashMap.elems threads
          --    return (f user, threads)
    case profileRequest of
      CheckPronouns newPronouns -> do
        checkResult <- if
          -- pronouns must be between 2 - 10 characters and  be alphanumeric or '/'
          | isPronounsCorrectFormat newPronouns -> return 2
          -- TODO moderate language
          | False -> return 3
          | otherwise -> return 1
        -- 1 == Valid
        -- 2 == Invalid format
        -- 3 == Invalid language
        jsonResponse "check_pronouns"
          ["check_result" .= (checkResult :: Int)]
      SetPronouns newPronouns -> do
        setResult <- if
          -- pronouns must be between 2 - 10 characters and  be alphanumeric or '/'
          | isPronounsCorrectFormat newPronouns -> return 2
          -- TODO moderate language
          | False -> return 3
          | otherwise -> return 1
        -- 1 == Valid
        -- 2 == Invalid format
        -- 3 == Invalid language
        when (setResult == 1) $ modifyCurrentUser
          [DB.UserPronouns =. Just newPronouns]
          $ set pronouns $ Just newPronouns
        jsonResponse "set_pronouns"
           ["set_result" .= (setResult :: Int)]
      EquipBadge position name ->
        if not $ HashSet.member name (_collection _badges)
              || any (\int -> int >=_season _accountInfo)
                     (readMay =<< Text.stripPrefix "Season " name)
           then jsonError "Invalid Badge Name"
           else do
             let (stmBadge, dbBadge, newFirstBadge, newSecondBadge) = case position of
                   1 -> (firstBadge, DB.UserFirstBadge
                        ,Just name, _secondBadge _badges)
                   _ -> (secondBadge, DB.UserSecondBadge
                        ,_firstBadge _badges, Just name)
             modifyCurrentUser
               [dbBadge  =. Just name]
               $ set (badges . stmBadge) $ Just name
             jsonResponse "equip_badge"
               ["first_badge" .= newFirstBadge
               ,"second_badge" .= newSecondBadge]
      UnEquipBadge position -> do
        let newFirstBadge = if position /= 1
              then _firstBadge _badges else _secondBadge _badges
            newSecondBadge = if position /= 2
              then _secondBadge _badges else Nothing
        modifyCurrentUser
          [DB.UserFirstBadge  =. newFirstBadge
          ,DB.UserSecondBadge =. newSecondBadge]
          $ over badges $ set firstBadge newFirstBadge
                        . set secondBadge newSecondBadge
        jsonResponse ("unequip_badge_" ++ showText position)
          ["first_badge" .= newFirstBadge
          ,"second_badge" .= newSecondBadge]
      SetDefaultColor maybeColor -> do
        modifyCurrentUser
          [DB.UserDefaultColor =. fmap showText maybeColor]
          $ over (nameColor . defaultNameColor) $ \color ->
              case (color, maybeColor) of
                (Left _, Nothing) -> color
                (Right c, Nothing) -> Left c
                (_, Just c) -> Right c
        case maybeColor of
            Just c ->
              jsonResponse "set_default_color"
                ["default_name_color" .= c
                ,"is_random" .= False]
            _ -> do
              rand <- randomDefaultColor $ _userCreationTime _accountInfo
              jsonResponse "set_default_color"
                ["default_name_color" .= rand
                  ,"is_random" .= True]
      SetNameColor newLeft newRight newMode -> do
        let validChromaValue (ChromaColor {..}) = foldr (&&) True
              [30 <= _chromaLight, _chromaLight <= 150
              ,30 <= _chromaDark,  _chromaDark  <= 150
              ,0  <= _valueLight,  _valueLight  <= 90
              ,25 <= _valueDark,   _valueDark   <= 100]
        if validChromaValue newLeft && validChromaValue newRight
           then do -- dont do anything, if nothing changed
             when (Just newLeft  /= _left  _nameColor
                || Just newRight /= _right _nameColor
                || newMode       /= _mode  _nameColor) $ void $ do
               modifyCurrentUser
                 [DB.UserColorMode    =. showText newMode
                 ,DB.UserColorLeftH   =. Just (_hue         newLeft)
                 ,DB.UserColorLeftCL  =. Just (_chromaLight newLeft)
                 ,DB.UserColorLeftCD  =. Just (_chromaDark  newLeft)
                 ,DB.UserColorLeftVL  =. Just (_valueLight  newLeft)
                 ,DB.UserColorLeftVD  =. Just (_valueDark   newLeft)
                 ,DB.UserColorRightH  =. Just (_hue         newRight)
                 ,DB.UserColorRightCL =. Just (_chromaLight newRight)
                 ,DB.UserColorRightCD =. Just (_chromaDark  newRight)
                 ,DB.UserColorRightVL =. Just (_valueLight  newRight)
                 ,DB.UserColorRightVD =. Just (_valueDark   newRight)]
                 $ set (nameColor . mode) newMode
             jsonResponse "set_name_color"
               ["left" .= newLeft
               ,"right" .= newRight
               ,"mode" .= newMode]
           else jsonError "Invalid Colors"
      --SetMode newMode -> do
      --  when (newMode /= _mode _nameColor) $ void $ do
      --    atomically $ modifyTVar tvUsersInChat $ flip HashMap.adjust _userId $
      --      set (nameColor . mode) newMode
      --    runDB $ P.update (P.toSqlKey _userId)
      --      [DB.UserColorMode =. showText newMode]
      --  return $ jsonResponse "set_mode" []



data ProfileRequest
  = CheckPronouns
    {newPronouns :: Text}
  | SetPronouns
    {newPronouns :: Text}
  | EquipBadge
    {badgePosition :: Word
    ,badgeName :: Text}
  | UnEquipBadge
    {badgePosition :: Word}
  | SetDefaultColor
    {defaultColor :: Maybe DefaultColor}
  | SetNameColor
    {left :: ChromaColor
    ,right :: ChromaColor
    ,colorMode :: ChromaMode}
  deriving (Show)

instance FromJSON ProfileRequest where
  parseJSON = withObject "ProfileRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "check_pronouns" ->
        CheckPronouns <$> obj .: "pronouns"
      "set_pronouns" ->
        SetPronouns <$> obj .: "pronouns"
      "equip_badge" -> asum
        [EquipBadge 1  <$> obj .: "equip_1"
        ,EquipBadge 2 <$> obj .: "equip_2"]
      "unequip_badge_1" -> return $ UnEquipBadge 1
      "unequip_badge_2" -> return $ UnEquipBadge 2
      "set_default_color" ->
        SetDefaultColor <$> obj .: "default_color"
      "set_name_color" ->
        SetNameColor <$> obj .: "left"
                     <*> obj .: "right"
                     <*> obj .: "mode"
      str -> fail $ "Do Not Recognize Profile Request Type: " ++ str








getLogOutR :: Handler Value
getLogOutR = do
  clearCreds False
  jsonResponse "log_out" []
