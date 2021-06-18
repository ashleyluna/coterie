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

import Internal.Api
import Internal.StreamerInfo
import Internal.User
import Internal.WS


getMyProfileR :: Handler Value
getMyProfileR = apiIfLoggedIn $ \User {..} -> do
  let AccountInfo {..} = _accountInfo
  role <- case _role of
    Right (SpecialRole {..}) -> return $ object
      ["name" .= _roleName
      ,"power" .= _power]
    Left (Chatter _months sub) -> do
      maybeSubscription <- flip (maybe $ return Null) sub $ \Subscription {..} -> do
        maybeGifterName <- f2map DB.userUsername $
          _maybeGifterId >>*= runDB . E.get . E.toSqlKey
        return $ object
          ["tier" .= _subTier
          ,"gifter" .= maybeGifterName
          ,"is_3_month_package" .= _is3MonthPackage
          ,"recurring" .= _recurring
          ,"end_time" .= _endTime
          ]
      return $ object
        ["months" .= _months
        ,"subscription" .= maybeSubscription
        ,"can_post_links" .= (isRight _role || isSafeUser _moderation)
        ]
  jsonResponse "get_profile"
    ["account" .= object
      ["email" .= _userEmail
      ,"num_months_subbed" .= _numMonthsSubbed
      ,"season" .= _season
      ,"twitch_conn" .= _twitchConn
      ,"google_conn" .= _googleConn
      ]
    ,"username" .= _username
    ,"role" .= role
    ,"badges" .= _badges
    ,"pronouns" .= _pronouns
    ,"name_color" .= object
      ["default_name_color" .= toJSON (case _defaultNameColor _nameColor of
        Left color -> show color
        Right color -> show color)
      ,"is_random" .= toJSON (case _defaultNameColor _nameColor of
        Left _ -> True
        _ -> False)
      ,"left" .= _left _nameColor
      ,"right" .= _right _nameColor
      ,"mode" .= show (_mode _nameColor)
      ]
    -- points
    ]








postMyProfileR :: Handler Value
postMyProfileR = do
  profileRequest <- (requireCheckJsonBody :: Handler ProfileRequest)
  apiIfLoggedIn $ \User {..} -> do
    App {..} <- getYesod
    let StreamerInfo {..} = streamerInfo
        modifyUserConns f = atomically $ do
          maybeUserConn <- HashMap.lookup _userId <$> readTVar tvUserConns
          case maybeUserConn of
            Just (tvCurrentUser,_) -> modifyTVar' tvCurrentUser f
            _ -> return ()
          --writeTVar tvUserConns =<< flip2 HashMap.alterF _userId userConns ¢
          --  traverse ¢ \(user, threads) -> do
          --    traverse (flip writeTQueue $ SelfUpdate $ f user) $ HashMap.elems threads
          --    return (f user, threads)
        updateUserDB = runDB . P.update (P.toSqlKey _userId)
    case profileRequest of
      CheckPronouns newPronouns -> do
        checkResult <- if not $ betweenLen 2 10 newPronouns
                             && Text.all (\c -> Char.isAlphaNum c || c == '/')
                                         newPronouns
                       then return 2
                       else if False -- TODO moderate language
                       then return 3
                       else return 1
        -- 1 == Valid
        -- 2 == Invalid format
        -- 3 == Invalid language
        jsonResponse "check_pronouns"
          ["check_result" .= (checkResult :: Int)]
      SetPronouns newPronouns ->
        if not $ betweenLen 2 10 newPronouns
              && Text.all (\c -> Char.isAlphaNum c || c == '/')
                          newPronouns
              && True -- TODO moderate language
           then jsonError "Invalid Pronouns"
           else do
             modifyUserConns $ set pronouns $ Just newPronouns
             updateUserDB
               [DB.UserPronouns =. Just newPronouns]
             jsonResponse "set_pronouns"
               ["pronouns" .= newPronouns]
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
             modifyUserConns $ set (badges . stmBadge) $ Just name
             updateUserDB [dbBadge  =. Just name]
             jsonResponse "equip_badge"
               ["first_badge" .= newFirstBadge
               ,"second_badge" .= newSecondBadge]
      UnEquipBadge position -> do
        let newFirstBadge = if position /= 1
              then _firstBadge _badges else _secondBadge _badges
            newSecondBadge = if position /= 2
              then _secondBadge _badges else Nothing
        modifyUserConns $ over badges $ set firstBadge newFirstBadge
                                    . set secondBadge newSecondBadge
        updateUserDB
          [DB.UserFirstBadge  =. newFirstBadge
          ,DB.UserSecondBadge =. newSecondBadge
          ]
        jsonResponse ("unequip_badge_" ++ showText position)
          ["first_badge" .= newFirstBadge
          ,"second_badge" .= newSecondBadge]
      SetDefaultColor maybeColor -> do
        modifyUserConns $ over (nameColor . defaultNameColor) $ \color ->
          case (color, maybeColor) of
            (Left _, Nothing) -> color
            (Right c, Nothing) -> Left c
            (_, Just c) -> Right c
        updateUserDB
          [DB.UserDefaultColor =. fmap showText maybeColor]
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
               modifyUserConns $ set (nameColor . mode) newMode
               updateUserDB
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
