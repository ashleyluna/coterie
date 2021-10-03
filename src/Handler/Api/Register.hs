{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Handler.Api.Register
  (postRegCheckR
  ,getRegTwitchR
  ) where

import Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as P
import System.Random as SyR
--import Text.Email.Validate as EmV
import Yesod.Auth.Util.PasswordStore as YPS






import Import
import qualified Model as DB

import Control.Lens hiding ((.=))
import Data.IntMap as IntMap
import Data.HashSet as HashSet
import Data.Maybe as Maybe
import Network.HTTP.Simple as SHttp
import Web.Cookie


import Postfoundation
import Internal.StreamerInfo
import Internal.Twitch
import Prefoundation
import Data.ByteString.Char8 (putStr)
import Yesod (getYesod)
import Import.NoFoundation (modifyTVar)


-- check username
postRegCheckR :: Handler Value
postRegCheckR = do
  CheckUsername requestedUsername <- (requireCheckJsonBody :: Handler CheckRequest)
  checkResult <- if not $ betweenLen 3 20 requestedUsername
                       && Text.all (\c -> Char.isAlphaNum c || c == '_')
                                   requestedUsername
                 then do delReqCookie
                         return 2
                 else if False --moderate name language
                 then do delReqCookie
                         return 3
                 else do
                 usernameExists <- return False
                             --    any ((requestedUsername ==) . _userUsername)
                             --  . Map.elems
                             --  . _users
                             -- <$> getBadDB --runDB $ isJust <$> getBy (UniqueUsername requestedUsername) -- DB.username reqUsername
                 if usernameExists
                 then do delReqCookie
                         return 4
                 else do expiration <- getExpires 3
                         setCookie $ defaultSetCookie
                           {setCookieName = "requestedUsername"
                           ,setCookieValue = encodeUtf8 requestedUsername
                           ,setCookiePath = Just "/"
                           ,setCookieExpires = Just expiration}
                         return 1
  -- 1 == Valid
  -- 2 == Invalid format
  -- 3 == Invalid language
  -- 4 == Name already in use
  jsonResponse "check_username"
    ["check_result" .= (checkResult :: Int)]

data CheckRequest
  = CheckUsername
    {checkUsername :: Text}
  deriving (Show, Generic, ToJSON)

instance FromJSON CheckRequest where
  parseJSON = withObject "CheckRequest" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: String) of
      "check_username" ->
        CheckUsername <$> obj .: "username"
      str -> fail $ "Do Not Recognize RegisterCheck Request Type: " ++ str




--------------------------------------------------------------------------------




getRegTwitchR :: Handler ()
getRegTwitchR = do
  app@App{..} <- getYesod
  let Secrets {..} = streamerSecrets
  void $ lookupGetParam "code" >>=- \code -> do
    renderUrl <- getUrlRender
    -- get auth tokens
    response <- SHttp.httpJSONEither $ setRequestQueryString
      [mkQuery "client_id" $ Text.encodeUtf8 twitchClientId
      ,mkQuery "client_secret" $ Text.encodeUtf8 twitchClientSecret
      ,mkQuery "code" $ Text.encodeUtf8 code
      ,mkQuery "grant_type" "authorization_code"
      ,mkQuery "redirect_uri" $ Text.encodeUtf8 $ renderUrl $ ApiR $ RegisterR RegTwitchR]
      "POST https://id.twitch.tv/oauth2/token"
    case getResponseBody response of
      Left _ -> do putStrLn "ERROR getRegTwitchR: invalid 'code'"
                   showTwitchResponseError response
      Right tokens -> getTwitchUserFull tokens >>= \case -- get twitch account info
        Nothing -> putStrLn "ERROR getRegTwitchR: idk why this failed"
        Just (TwitchAuthInfo TwitchTokens{..} TwitchAccount{..}
             ,TwitchUser {..}, twitchFollowedAt, maybeTwitchSub) -> do
          let setTwitchCreds = setCreds True $ Creds "twitch" twitchId []
              --seasonBadges = flip (maybe "") maybeTwitchFollowedAt $ \followedAt ->
              --  unwords $ Maybe.catMaybes $ flip intmap $ \int seasonEndTime ->
              --    if followedAt <= seasonEndTime
              --       then Just $ "season" ++ showText int
              --       else Nothing
              updateUserTwitchInfo userId = do
                user <- runDB $ P.get userId
                currentTime <- getCurrentTime
                let newSeason = fromMaybe (List.length $ seasonEndTimes streamerInfo) $ do
                      oldSeason <- DB.userSeason <$> user
                      return $ min oldSeason $ assignSeason $ fromMaybe currentTime twitchFollowedAt
                runDB $ (DB.userTwitchAuth =<< user) `for_` flip P.update 
                  [DB.TwitchAuthAccessToken  =. accessToken
                  ,DB.TwitchAuthRefreshToken =. refreshToken
                  ,DB.TwitchAuthUserId       =. twitchId
                  ,DB.TwitchAuthLogin        =. twitchLogin
                  ,DB.TwitchAuthEmail        =. twitchEmail
                  ,DB.TwitchAuthCreatedTime  =. createdAt
                  ,DB.TwitchAuthFollowTime   =. twitchFollowedAt
                  ,DB.TwitchAuthIsSubscriber =. maybeTwitchSub
                  ]
                modifyUser tvUsers (P.fromSqlKey userId)
                  [DB.UserSeason             =. newSeason
                  --,UserBadgeCollection    =. seasonBadges
                  ] $
                  set (accountInfo . season) newSeason
          -- get current user and account associated with twitch info
          maybeUserId <- maybeAuthId
          maybeUser <- runDB $ getByUserAuth TwitchAuth twitchId
          case (maybeUserId, maybeUser) of
            -- logged in, twitch account in use
            -- whether its same connected account, or different,
            -- this resets the tokens of the coterie account with the associated twitch account
            -- we update the tokens, but if its a different connected account
            -- the user should log out before changing to a new account
            (Just _, Just (Entity userId _)) -> updateUserTwitchInfo userId
            -- logged in, twitch account in use, different connected account
            -- already logged in, twitch account not used
            -- add connection to account
            (Just userId, Nothing) -> updateUserTwitchInfo userId
            -- not logged in, twitch account in use
            -- log in
            (Nothing, Just _) -> setTwitchCreds
            -- not logged in, twitch account not used
            -- make new account
            (Nothing, Nothing) -> void $
              lookupCookie "requestedUsername" >>=- \requestedUsername -> do
                delReqCookie
                -- add user to database
                newUserDB <- newUserDB twitchEmail requestedUsername
                let newTwitchAuth = DB.TwitchAuth
                      {DB.twitchAuthAccessToken  = accessToken
                      ,DB.twitchAuthRefreshToken = refreshToken
                      ,DB.twitchAuthUserId       = twitchId
                      ,DB.twitchAuthLogin        = twitchLogin
                      ,DB.twitchAuthEmail        = twitchEmail
                      ,DB.twitchAuthCreatedTime  = createdAt
                      ,DB.twitchAuthFollowTime   = twitchFollowedAt
                      ,DB.twitchAuthIsSubscriber = maybeTwitchSub
                      }
                userKey <- runDB $ do
                  twitchAuthKey <- P.insert newTwitchAuth
                  P.insert newUserDB
                    {DB.userTwitchAuth = Just twitchAuthKey}
                -- take user info from databse and add to stm memory
                newUser@User{..} <- fromUserDB app (Entity userKey newUserDB)
                tvNewUser <- newTVarIO newUser
                modifyTVarIO tvUsers $ HashMap.insert _userId tvNewUser
                  
                setTwitchCreds


       --     -- login
       --     Just _ -> setCreds True $ Creds "twitch" twitchId []
       --     -- sign up
       --       -- finally make account

       --   return ()
       --   --newUser <- set (accountInfo . twitchConn) True
       --   --  <$> newUser email requestedUsername
       --   --LiveInfo {..} <- liveInfo <$> getYesod
       --   --userJoin tvUsersInChat newUser -- TODO this shouldnt happen here, only temporary
       --   --setCreds False $ Creds "Twitch"
       --   --  (Text.pack $ show $ _userId newUser) []
       --   ---- TODO add user to database
  redirect HomePageR



--------------------------------------------------------------------------------


newUserDB :: MonadIO m => Text -> Text -> m DB.User
newUserDB email username = do
  --id <- liftIO SyR.randomIO
  creationTime <- getCurrentTime
  let season = assignSeason $ creationTime `div` 1000000
  return $ DB.User
    -- Account
    {userCreationTime       = creationTime
    ,userEmail              = email
    ,userLastNameChangeTime = creationTime
    ,userNumMonthsSubbed    = 0
    ,userSeason             = season
    -- Connections
    ,userTwitchAuth = Nothing
    ,userGoogleAuth = Nothing
    -- Moderation
    ,userMeaningfulMessages = 0
    -- Profile
    ,userUsername = username
    ,userRole = Nothing
    ,userPronouns = Nothing 
    ,userPoints = 0
    -- Badges
    ,userFirstBadge = Nothing
    ,userSecondBadge = Nothing
    ,userBadgeCollection = ""
    -- Name Color
    ,userDefaultColor = Nothing
    ,userColorMode = "LRGB"
    -- Left
    ,userColorLeftH   = Nothing
    ,userColorLeftCL  = Nothing
    ,userColorLeftCD  = Nothing
    ,userColorLeftVL  = Nothing
    ,userColorLeftVD  = Nothing
    -- Right
    ,userColorRightH  = Nothing
    ,userColorRightCL = Nothing
    ,userColorRightCD = Nothing
    ,userColorRightVL = Nothing
    ,userColorRightVD = Nothing
   }


delReqCookie = deleteCookie "requestedUsername" "/"




{-

data RegisterRequest
  = CheckUsername
    {_usernameCheck :: Text}
  | CheckEmail
    {_emailCheck :: Text}
  | CheckPassword
    {_passwordCheck :: Text}
  | SignUp
    {_usernameRegister :: Text
    ,_emailRegister    :: Text
    ,_passwordRegister :: Text
    }
  | GoogleSignIn
    {_code :: Text
    }
  deriving (Show, Generic, ToJSON)

instance FromJSON RegisterRequest where
  parseJSON = withObject "RegisterRequest" $ \obj -> do
    requestType <- obj .: "request"
    case (requestType :: Text) of
      "checkUsername" ->
        CheckUsername <$> obj .: "username"
      "checkEmail" ->
        CheckEmail <$> obj .: "email"
      "checkPassword" ->
        CheckPassword <$> obj .: "password"
      "signUp" ->
        SignUp <$> obj .: "username"
               <*> obj .: "email"
               <*> obj .: "password"
      "googleSignIn" ->
        GoogleSignIn <$> obj .: "code"
      _ -> fail "Invalid RegisterRequest request type"



postRegisterR :: Handler Value
postRegisterR = do
  maybeToken <- fmap reqToken getRequest
  case maybeToken of
    Nothing -> return ()
    Just token -> liftIO $ TextIO.putStrLn $ "\n" ++ token ++ "\n"
  request <- (requireCheckJsonBody :: Handler RegisterRequest)
  let checkResponse :: Text -> Text -> Int -> Handler Value
      checkResponse responseName fieldName int = return $ object
        ["response" .= responseName
        ,fieldName .= int]
  case request of
    CheckUsername str -> checkUsername str >>= checkResponse "checkUsername" "username"
    CheckEmail str -> checkEmail str >>= checkResponse "checkEmail" "email"
    CheckPassword str -> checkPassword str & checkResponse "checkPassword" "password"
                         -- ^ checkPassword doesnt do any IO, so we wont use bind
    SignUp username email password -> do
      checkUsernameNum <- checkUsername username
      checkEmailNum <- checkEmail email
      let checkPasswordNum = checkPassword password
          isValidSignUp = Import.all (== 0)
            [checkUsernameNum, checkEmailNum, checkPasswordNum]
      when isValidSignUp $ do
        -- TODO send confirmation email first
        creationTime <- liftIO $ round <$> (*1000) <$> POSIX.getPOSIXTime
        newId <- liftIO $ Text.pack . show <$> (SyR.randomIO :: IO Word64)
        hashedPassword <- liftIO $ Text.decodeUtf8 <$>
          YPS.makePassword (Text.encodeUtf8 password) 16
        --setSession "userData" <=<
        return ()
        --runDB $ do
        --  --Just chatter <- get $ RoleId 1
        --  insert $
        --    User newId creationTime email (Just hashedPassword) creationTime
        --         (RoleId 1) Nothing Nothing
        --         username Nothing Nothing Nothing 0 0
        --         False False True
        --runBeam $ do
        --  Bm.runInsert $ Bm.insert (DB._users DB.beamDatabaseDb) $
        --    Bm.insertExpressions $
        --    let chatter = DB.RoleId "Chatter" :: DB.RoleId
        --    in [DB.User Bm.default_ (val_ creationTime) (val_ email)
        --         (val_ $ Just hashedPassword) (val_ creationTime)
        --         (val_ chatter) (val_ Nothing) (val_ Nothing)
        --         (val_ username) (val_ Nothing) (val_ Nothing) (val_ Nothing)
        --         (val_ 0) (val_ 0)
        --         (val_ False) (val_ False) (val_ True)]
          --Bm.runSelectReturningOne $ Bm.select $
          --  Bm.filter_ (\user -> user ^. DB.username ==. Bm.val_ username) $
          --    Bm.all_ $ DB.beamDatabaseDb ^. DB.users
      return $ object
        ["response" .= ("signUp" :: Text)
        ,"checkUsername" .= checkUsernameNum
        ,"checkEmail" .= checkEmailNum
        ,"checkPassword" .= checkPasswordNum]
      --usernameExists <- runCheck DB.username usernameCheck
      --if usernameExists
      --then return $ object ["usernameExists" .= usernameExists]
      --else do
      --  emailExists <- runCheck DB.email emailCheck
      --  if emailExists
      --  then return $ object ["emailExists" .= emailExists]
      --  else do
      --    if isValidPassword passwordCheck
      --    then return $ object ["usernameExists" .= usernameExists]
      --    else return $ object []
    GoogleSignIn code -> do
      putStrLn code
      return $ object
        ["response" .= ("confirmed duck" :: Text)]

checkUsername :: Text -> Handler Int
checkUsername str =
  let isValidFormat = Text.length str >= 3
                   && Text.length str <= 24
                   && Text.all Char.isAlphaNum str
  in if not isValidFormat
     then return 1
     else do usernameExists <- any ((str ==) . _userUsername)
                             . Map.elems
                             . _users
                           <$> getBadDB --runDB $ isJust <$> getBy (UniqueUsername str) -- DB.username str
             if usernameExists
             then return 2
             else --moderate name language
                  return 0

checkEmail :: Text -> Handler Int
checkEmail str =
  let isValidFormat = EmV.isValid $ Text.encodeUtf8 str
  in if not isValidFormat
     then return 1
     else do emailExists <- any  ((str ==) . _userEmail)
                          . Map.elems
                          . _users
                        <$> getBadDB --runDB $ isJust <$> getBy (UniqueEmail str)
             if emailExists
                then return 2
                else --moderate email language
                     return 0

checkPassword :: Text -> Int
checkPassword str =
  let isValidFormat = Text.length str >= 8
                   && Text.length str <= 64
  in if not isValidFormat
     then 1
     else --moderate password language
          0



--runCheckDB field value = runBeam $ map isJust
--  $ Bm.runSelectReturningOne $ Bm.select
--    $ Bm.filter_ (\user -> user ^. field ==. Bm.val_ value)
--      $ Bm.all_ $ DB.beamDatabaseDb ^. DB.users


-}
