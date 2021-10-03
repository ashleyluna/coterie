{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Internal.Twitch where

import Import

import Control.Monad.Trans.Maybe
import Data.Text as Text hiding (any)
import Network.HTTP.Simple as SHttp
import System.Random as SyR
import Web.Cookie


import Prefoundation
import Streamer




-- WebHook
--streamStatusTwitch :: App -> IO ()
--streamStatusTwitch (App {..}) = do
--  let Secrets {..} = secrets streamerInfo
--  callback <- getUrlRender <*> return ApiR (StreamStatusR StreamSTwitchR)
--  secret <- showText <$> SyR.randomIO :: IO Word
--  returnBody <=< SHttp.httpJSONEither
--    $ setRequestBodyJSON ¢ object
--        ["type" .= ("stream.online" :: Text)
--        ,"version" .= ("1" :: Text)
--        ,"condition" .= object
--          ["broadcaster_user_id" .= twitchStreamerId]
--        ,"transport" .= object
--          ["method" .= ("webhook" :: Text)
--          ,"callback" .= callback
--          ,"secret" .= secret]]
--    $ setRequestHeader "Client-Id" [encodeUtf8 twitchClientId]
--      "POST https://api.twitch.tv/helix/eventsub/subscriptions"
--
--
--data TwitchStreamOnline




--------------------------------------------------------------------------------
-- Twitch Api


-- automatically validated
getTwitchUserFull :: MonadIO m => TwitchTokens -> m (Maybe (TwitchAuthInfo, TwitchUser, Maybe Int, Maybe Int))
getTwitchUserFull tokens = validateToken tokens >>= \case
  TwitchRevoked -> do putStrLn "Plz Revoke" -- TODO Revoke
                      return Nothing
  userAuthInfo@(TwitchAuthInfo tokens TwitchAccount{..}) -> 
    getTwitchUser_ tokens >>== \twitchUser -> do
      twitchFollowInfo <- getTwitchFollowInfo_ tokens twitchId
      twitchSubInfo <- getTwitchSubInfo_ tokens twitchId
      return $ Just (userAuthInfo, twitchUser
                    ,followedAt <$> twitchFollowInfo
                    ,twitchSubTier <$> twitchSubInfo)




-- automatically validated
getTwitchUser :: MonadIO m => TwitchTokens -> m (Maybe (TwitchAuthInfo, TwitchUser))
getTwitchUser tokens = validateToken tokens >>= \case
  TwitchRevoked -> do putStrLn "Plz Revoke" -- TODO Revoke
                      return Nothing
  userAuthInfo@(TwitchAuthInfo tokens _) -> runMaybeT $
    MaybeT (getTwitchUser_ tokens) <&> \twitchUser ->
      (userAuthInfo, twitchUser)




getTwitchFollowInfo_ :: MonadIO m => TwitchTokens -> Text -> m (Maybe TwitchFollowInfo)
getTwitchFollowInfo_ (TwitchTokens {..}) userTwitchId =
  let Secrets {..} = streamerSecrets
  in returnBody <=< SHttp.httpJSONEither
       $ setRequestHeader "Authorization" ["Bearer " ++ encodeUtf8 accessToken]
       $ setRequestHeader "Client-Id" [encodeUtf8 twitchClientId]
       $ SHttp.parseRequest_
       $ "GET https://api.twitch.tv/helix/follows?to_id=" ++ Text.unpack twitchStreamerId
                                           ++ "&from_id=" ++ Text.unpack userTwitchId




getTwitchSubInfo_ :: MonadIO m => TwitchTokens -> Text -> m (Maybe TwitchSubInfo)
getTwitchSubInfo_ (TwitchTokens {..}) userTwitchId =
  let Secrets {..} = streamerSecrets
  in returnBody <=< SHttp.httpJSONEither
       $ setRequestHeader "Authorization" ["Bearer " ++ encodeUtf8 accessToken]
       $ setRequestHeader "Client-Id" [encodeUtf8 twitchClientId]
       $ SHttp.parseRequest_
       $ "GET https://api.twitch.tv/helix/subscriptions/user?broadcaster_id=" ++ Text.unpack twitchStreamerId
                                                               ++ "&user_id=" ++ Text.unpack userTwitchId




-- without auto validation
getTwitchUser_ :: MonadIO m => TwitchTokens -> m (Maybe TwitchUser)
getTwitchUser_ (TwitchTokens {..}) =
  let Secrets {..} = streamerSecrets
  in returnBody <=< SHttp.httpJSONEither
       $ setRequestHeader "Authorization" ["Bearer " ++ encodeUtf8 accessToken]
       $ setRequestHeader "Client-Id" [encodeUtf8 twitchClientId]
         "GET https://api.twitch.tv/helix/users"




validateToken :: MonadIO m => TwitchTokens -> m TwitchAuthInfo
validateToken tokens@TwitchTokens{..} = do
  let Secrets {..} = streamerSecrets
  response1 <- SHttp.httpJSONEither
    $ setRequestHeader "Authorization" ["OAuth " ++ encodeUtf8 accessToken]
      "GET https://id.twitch.tv/oauth2/validate"
  case (responseBody response1 :: Either JSONException TwitchAccount) of
    Right twitchAccount -> return $ TwitchAuthInfo tokens twitchAccount
    Left _ -> do -- refresh access token
      response2 <- SHttp.httpJSONEither $ setRequestQueryString
          [mkQuery "grant_type" "refresh_token"
          ,mkQuery "refresh_token" $ encodeUtf8 refreshToken
          ,mkQuery "client_id" $ encodeUtf8 twitchClientId
          ,mkQuery "client_secret" $ encodeUtf8 twitchClientSecret
          ]
          "POST https://id.twitch.tv/oauth2/token--data-urlencode"
      case (responseBody response2 :: Either JSONException TwitchTokens) of
        Right tokens -> validateToken tokens
        Left _ -> do
          showTwitchResponseError response1
          showTwitchResponseError response2
          return TwitchRevoked -- user revoked connection or unknown error






data TwitchAuthInfo
  = TwitchAuthInfo
    {validAccessToken :: TwitchTokens
    ,twitchAccount :: TwitchAccount}
  | TwitchRevoked

-- from refresh https://id.twitch.tv/oauth2/token
data TwitchTokens = TwitchTokens
  {accessToken :: Text
  ,refreshToken :: Text
  } deriving Show
instance FromJSON TwitchTokens where
  parseJSON = withObject "TwitchTokens" $ \obj ->
    TwitchTokens <$> obj .: "access_token"
                 <*> obj .: "refresh_token"

-- from validate https://id.twitch.tv/oauth2/validate
data TwitchAccount = TwitchAccount
    {twitchId :: Text
    ,twitchLogin :: Text
    } deriving Show
instance FromJSON TwitchAccount where
  parseJSON = withObject "TwitchAccount" $ \obj ->
    TwitchAccount <$> obj .: "user_id"
                  <*> obj .: "login"


data TwitchUser = TwitchUser
  {twitchEmail :: Text
  ,createdAt :: Int
  ,profileImageUrl :: Text
  } deriving Show
instance FromJSON TwitchUser where
  parseJSON = withObject "TwitchUser" $ \obj -> do
    users <- obj .: "data"
    case users of
      [] -> fail "No User Provided"
      obj:_ -> TwitchUser <$> obj .: "email"
                          <*> (obj .: "created_at" >>= rfc3339Json)
                          <*> obj .: "profile_image_url"

newtype TwitchFollowInfo = TwitchFollowInfo
  {followedAt :: Int
  } deriving Show
instance FromJSON TwitchFollowInfo where
  parseJSON = withObject "TwitchFollowInfo" $ \obj -> do
    users <- obj .: "data"
    case users of
      [] -> fail "No Follow Info Provided"
      obj:_ -> fmap TwitchFollowInfo $ obj .: "followed_at" >>= rfc3339Json

newtype TwitchSubInfo = TwitchSubInfo
  {twitchSubTier :: Int
  } deriving Show
instance FromJSON TwitchSubInfo where
  parseJSON = withObject "TwitchSubInfo" $ \obj -> do
    users <- obj .: "data"
    case users of
      [] -> fail "No Subscription Info Provided"
      obj:_ -> obj .: "tier" <&> \tier -> TwitchSubInfo $
        case tier :: Text of
          "1000" -> 1
          "2000" -> 2
          _ -> 3




returnBody :: (MonadIO m, Show a1, Show a2)
           => Response (Either a1 a2) -> m (Maybe a2)
returnBody response = case responseBody response of
  Right x -> return $ Just x
  Left _ -> do showTwitchResponseError response
               return Nothing


showTwitchResponseError :: (MonadIO m, Show a) => Response a -> m ()
showTwitchResponseError response = case getResponseStatusCode response of
  400 -> do putStrLn "ERROR Twitch Request: Bad Request\n"
            putStrLn $ Text.pack $ show response -- unknown error
  401 -> do putStrLn "ERROR Twitch Request: Unauthorized\n"
            putStrLn $ Text.pack $ show response -- unknown error
  -- 404
  404 -> return () -- ex: checked if user follows streamer, but they dont
  _ -> do putStrLn $ Text.pack $ show response



rfc3339Json :: MonadFail m => Text -> m Int
rfc3339Json rfc3339Time = case rfc3339ToPOSIXSeconds rfc3339Time of
  Just a -> return a
  _ -> fail "Failed To Parse RFC3339 Format"









{-

streamStatusProcess :: StreamerInfo
                    -> TChan StreamStatus
                    -> TVar StreamStatus
                    -> IO ()
streamStatusProcess streamerInfo streamStatusTChan tVarStreamStatus = void $ forkIO $ do
  putStrLn "start"
  withSocketsDo $ WS.runClient "pubsub-edge.twitch.tv" 80 "/" $ \conn -> do
    putStrLn "2"
    --let ping = do threadDelay $ 4.5 * 1000000
    --              sendTextData conn $ encode object ["type" .= ("PING" :: Text)]
    WS.sendTextData conn $ encode $ object ["type" .= ("PING" :: Text)]
    msg <- WS.receiveData conn
    putStrLn msg
    WS.sendClose conn ("close" :: ByteString)
  putStrLn "done"
  --let twitchWS :: IO ()
  --    twitchWS = connect
  --      (decode' `mapOutput` --mapOutputMaybe
  --        httpSource (parseRequest_ "GET wss://pubsub-edge.twitch.tv") getResponseBody)
  --      $ awaitForever $ \twitchWSResponse -> case twitchWSResponse of
  --      _ -> liftIO (putStrLn "decodeUtf8")
  --twitchWS
  --response <- httpJSON =<<
  --      setRequestHeaders
  --        [pair "Client-ID" $ twitchClientId streamerInfo
  --        ,pair "Authorization" $ "Bearer " ++ twitchClientSecret streamerInfo
  --        ,pair "Content-Type" "application/json"
  --        ]
  --  <$> setRequestBodyJSON (object
  --        ["type" .= ("theserfstv.follow" :: Text)
  --        ,"version" .= ("1" :: Text)
  --        ,"condition" .=
  --        ,"transport" .=
  --        ])
  --  <$> parseRequest "POST https://api.twitch.tv/helix/eventsub/subscriptions"
  --threadDelay 2000000
  --putStrLn "we got here"
  --response <- httpJSON "POST https://api.twitch.tv/helix/eventsub/subscriptions"
  --
  --putStrLn "we ended there"





data TwitchWSResponse
  = Pong
  | Reconnect

instance FromJSON TwitchWSResponse where
  parseJSON = withObject "TwitchWSResponse" $ \obj -> do
    requestType <- obj .: "type"
    case (requestType :: Text) of
      "PONG" -> return Pong
      "RECONNECT" -> return Reconnect
      _ -> fail "Invalid TwitchWSResponse response type"
-}
