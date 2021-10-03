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



runDB_ :: MonadIO m => App -> SqlPersistT IO a -> m a
runDB_ master action = liftIO $ P.runSqlPool action $ appConnPool master


getSub :: Key DB.User -> Handler (Maybe (Entity DB.ActiveSub))
getSub = runDB . P.getBy . DB.UniqueSubber

apiIfLoggedIn :: (TVUser -> User -> Handler Value) -> Handler Value
apiIfLoggedIn f = getCurrentUser >>= \case
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


fromUserDB :: MonadIO m => App -> Entity DB.User -> m User
fromUserDB app@App{..} userEntity@(Entity userKey user@DB.User{..}) = do
  let userId = E.fromSqlKey userKey
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
  accountInfo <- do
    twitchConn <- userTwitchAuth ->== runDB_ app . get >>=- \DB.TwitchAuth{..} -> 
      return $ TwitchConn twitchAuthUserId twitchAuthAccessToken twitchAuthRefreshToken
                          twitchAuthLogin twitchAuthEmail twitchAuthCreatedTime
                          twitchAuthFollowTime twitchAuthIsSubscriber
    return $ AccountInfo userCreationTime userEmail userLastNameChangeTime
                         userNumMonthsSubbed userSeason
                         twitchConn
    --() -- google conn
  defaultNameColor <- case userDefaultColor >>= readMay of
    Just color -> return $ Right color
    _ -> Left <$> randomDefaultColor userCreationTime
  specialRoles <- liftIO $ readTVarIO tvSpecialRoles
  role <- case userRole >>= flip HashMap.lookup specialRoles . E.fromSqlKey of
    Just specialRole -> return $ Right specialRole
    _ -> return(Left $ Chatter userNumMonthsSubbed Nothing) -- NOTE 1
  let nameColor = NameColor defaultNameColor
                            maybeLeft
                            maybeRight
                          $ fromMaybe LRGB $ readMay userColorMode
  creatorInfo <- HashMap.lookup userId <$> readTVarIO tvCreators
  moderation <- UserModeration <$> return userMeaningfulMessages
                               <*> return []
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




{-
1:
  We don't add subscription information with fromUserDB beacause Subscription
  needs TVUser for the user user who owns the subscription and the possible
  gifter. Since fromUserDB should only be used for setUpDatabaseStandIns
  and account creation, we just add the subscription information later.

-}