{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Postfoundation.Api where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql as P

import Import
import qualified Model as DB

import Prefoundation


getSub :: Key DB.User -> Handler (Maybe (Entity DB.ActiveSub))
getSub = runDB . P.getBy . DB.UniqueSubber

apiIfLoggedIn :: (TVUser -> User -> Handler Value) -> Handler Value
apiIfLoggedIn f = currentUser >>= \case
  Nothing -> jsonError "Not Logged In"
  Just (tvUser, user) -> f tvUser user

apiIfNoProfanity :: p -> Handler Value -> Handler Value
apiIfNoProfanity str m =
  if False then jsonError "Profanity"
     else m


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- LiveInfo




{-

fromUserDB_ :: Entity DB.User -> Handler User
fromUserDB_ userEntity@(Entity userKey user@(DB.User {..})) = do
  app@App{..} <- getYesod
  let userId = E.fromSqlKey userKey
      accountInfo = AccountInfo userCreationTime userEmail
                                userLastNameChangeTime userNumMonthsSubbed
                                userSeason
        -- twitch Connection
        (isJust userTwitchId)
        -- google Connection
        (isJust userGoogleId)
      badges = BadgeCollection userFirstBadge userSecondBadge $
        HashSet.fromList $ words userBadgeCollection
      maybeLeft = ChromaColor <$> userColorLeftH
                              <*> userColorLeftCL
                              <*> userColorLeftCD
                              <*> userColorLeftVL
                              <*> userColorLeftVD
      maybeRight = ChromaColor <$> userColorRightH
                               <*> userColorRightCL
                               <*> userColorRightCD
                               <*> userColorRightVL
                               <*> userColorRightVD
  defaultNameColor <- case userDefaultColor >>= readMay of
    Just color -> return $ Right color
    _ -> Left <$> randomDefaultColor userCreationTime
  specialRoles <- liftIO $ readTVarIO tvSpecialRoles
  role <- case userRole >>= flip HashMap.lookup specialRoles . E.fromSqlKey of
    Just specialRole -> return $ Right specialRole
    _ -> liftIO $ (Left . Chatter userNumMonthsSubbed) .
                  -- check if user has a subscription
                  HashMap.lookup userId <$> readTVarIO tvSubscriptions
  let nameColor = NameColor defaultNameColor
                            maybeLeft
                            maybeRight
                          $ fromMaybe LRGB $ readMay userColorMode
  creatorInfo <- HashMap.lookup userId <$> readTVarIO tvCreators
  moderation <- UserModeration <$> return userMeaningfulMessages
                               <*> return 0
                               <*> return False
  return $ User userId
                accountInfo
                creatorInfo
                moderation
                userUsername
                userPronouns
                role
                badges
                nameColor
                -- points

-}


fromRoleDB :: Entity DB.Role -> SpecialRole
fromRoleDB (Entity key DB.Role {..}) = SpecialRole (fromSqlKey key) roleName rolePower roleOrder


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- ModerationInfo






--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Stand In For Database
