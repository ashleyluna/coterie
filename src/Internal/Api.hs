{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.Api where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql as P

import Import
import qualified Model as DB

import Internal.User

getUser :: Text -> Handler (Maybe (Entity DB.User))
getUser = runDB . P.getBy . DB.UniqueUsername

getSub :: Key DB.User -> Handler (Maybe (Entity DB.ActiveSub))
getSub = runDB . P.getBy . DB.UniqueSubber

apiIfLoggedIn :: Cont (Handler Value) User
apiIfLoggedIn f = currentUser >>= \case
  Nothing -> jsonError "Not Logged In"
  Just user -> f user

apiIfNoProfanity str m =
  if False then jsonError "Profanity"
     else m


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- LiveInfo



currentUser :: Handler (Maybe User)
currentUser = do
  maybeAuth >$>= fromUserDB

findUser :: Text -> Handler (Maybe User)
findUser username = runDB (P.getBy $ DB.UniqueUsername username) >$>= fromUserDB

fromUserDB :: Entity DB.User -> Handler User
fromUserDB userEntity@(Entity userKey user@(DB.User {..})) = do
  App {..} <- getYesod
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
  role <- case E.fromSqlKey <$> userRole >>= flip HashMap.lookup specialRoles of
    Just specialRole -> return $ Right specialRole
    _ -> liftIO $ Left . Chatter userNumMonthsSubbed <$>
                  -- check if user has a subscription
                  HashMap.lookup userId <$> readTVarIO tvSubscriptions
  let nameColor = NameColor defaultNameColor
                            maybeLeft
                            maybeRight
                          $ fromMaybe LRGB $ readMay userColorMode
  creatorInfo <- HashMap.lookup userId <$> readTVarIO tvCreators
  moderation <- UserModeration <$> return 0
                               <*> return 0
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




fromRoleDB :: Entity DB.Role -> SpecialRole
fromRoleDB (Entity _ (DB.Role {..})) = SpecialRole roleName rolePower


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
