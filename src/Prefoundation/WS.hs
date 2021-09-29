{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Prefoundation.WS where


import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map
import Control.Monad.Fail
import Control.Applicative
import Control.Concurrent
import Control.Lens hiding ((.=))
--import Control.Concurrent.STM
--import Control.Concurrent.STM.Lifted
import Data.Aeson
import Data.Aeson.Text
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text hiding (empty, foldr)
import Data.Text.Lazy as TextL (toStrict)
import UnliftIO.Async
import Yesod.WebSockets

--import OrganizeLater
--import BadDatabase as BD

--import qualified Data.Text.Lazy as TL
--import Control.Monad (forever)
--import Control.Monad.Trans.Reader
--import Control.Concurrent (threadDelay)
--import Data.Time
--import Conduit
--import Data.Monoid ((<>))
--import Control.Concurrent.STM.Lifted
--import Data.Text (Text)

import Import.NoFoundation
import Streamer

import Prefoundation.User




-- global event subscription
data GESub = GESub
  {receiveMainChat     :: Bool
  ,receiveStreamStatus :: Bool
  --,recieveShoutOutBox  :: Bool -- home and maybe chat pages
  ,recieveModEvents    :: Bool -- all mods not on chat stream page
  --,recieveSubDono      :: Bool -- admin/streamer pages
  } deriving Show

data GlobalEvent
  = MainChatGlobalEvent ChatEvent
  | StreamStatusGlobalEvent StreamStatus
  | ModGlobalEvent ModEvent




data StreamStatus
  = Streaming
      {_stream :: StreamingService
      ,_title :: Text
      ,_startTime :: Int -- start = stream started at 2 pm, Int = number of seconds in Posix
      ,_viewerCount :: Int}
   | Hosting
       {_stream :: StreamingService
       ,_title :: Text}
   | Offline

data StreamingService = TwitchStream Text
                      | YouTubeStream Text

isStreaming :: StreamStatus -> Bool
isStreaming Streaming{..} = True
isStreaming _ = False




data ModEvent
  = AddDelayedUserMessage
      {userInfoDelayed :: User
      ,userMessageDelayed :: UserMessage}
  | ModChat ChatEvent


data ChatEvent
  = AddUserMessage
      {userInfo :: User
      ,userMessage :: UserMessage}
  | RemoveUserMessage
      {userUsername :: Text
      ,userMessage :: UserMessage}
  | AddUser
      {userInfo :: User}
  | RemoveUser
      {userUsername :: Text}
  | MuteUser
     {userUsername :: Text} -- removes all messages, ban or mute
  | SetUserList
      {userList :: HashMap Text User}











data Notification
  = Noti

instance ToJSON Notification where
  toJSON noti = object []




--------------------------------------------------------------------------------

data WSRequest
  = GESubscription GESub
--  | GetStreamStatus
--  | GetUsersInChat
--  | UserReq UserRequest -- requests that require log in
--
--data UserRequest
--  = MainChatMessageRequest
--      {message :: Text}
--  | ModChatMessageRequest
--      {message :: Text}
--  | WhisperMessageRequest
--      {receiver :: Text
--      ,message :: Text}
--  | ModerationRequest ModerationRequest -- must be mod
--
--data ModerationRequest
--  = UserInfo
--  | ModAction ModAction
--
--
--data ModAction
--  = Mute Text Word -- username time length
--  | Ban Text -- username



instance FromJSON WSRequest where
  parseJSON = withObject "WSRequest" $ \obj -> obj .: "type" >>= \case
    "gesub" -> fmap GESubscription $
      GESub <$> obj .: "main_chat"
            <*> obj .: "stream_status"
            <*> obj .: "mod"
    --"get_stream_status" -> return GetStreamStatus
    --"get_users_in_main_chat" -> return GetUsersInChat
    --"message" -> fmap UserReq $ obj .: "type_chat" >>= \case
    --  "main" -> MainChatMessageRequest <$> obj .: "message"
    --  "mod" -> ModChatMessageRequest <$> obj .: "message"
    --  str -> fail $ "Do Not Recognize Chat Type Request Type: " ++ str
    str -> fail $ "Do Not Recognize WS Request Type: " ++ str



--------------------------------------------------------------------------------
-- simple JSON
-- we have simple versions so mods & admins can look at the full info



makeLenses ''StreamStatus
