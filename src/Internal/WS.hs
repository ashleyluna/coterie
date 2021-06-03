{-# LANGUAGE DeriveAnyClass #-}

module Internal.WS where


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

import Internal.Images
import Internal.Payment
import Internal.User




-- global event subscription
data GESub = GESub
  {receiveMainChat     :: Bool
  ,receiveStreamStatus :: Bool
  --,recieveShoutOutBox  :: Bool -- home and maybe chat pages
  ,recieveModEvents    :: Bool -- all mods not on chat stream page
  --,recieveSubDono      :: Bool -- admin/streamer pages
  } deriving Show

data GlobalEvent
  = MainChat ChatMessage
  | StreamStatus StreamStatus
  | ModEvent ModEvent
 -- | Mod (Either ModAction ChatMessage)


instance ToJSON GlobalEvent where
  toJSON globalEvent = case globalEvent of
    MainChat chatMessage -> object $
      ["type" .= ("main_chat" :: Text)]
      ++ chatMessageJson chatMessage
    StreamStatus streamStatus -> toJSON streamStatus
    ModEvent modEvent -> toJSON modEvent




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

instance ToJSON StreamStatus where
  toJSON status =
    let serviceJSON stream = case stream of
          TwitchStream stream -> "twitch" .= stream
          YouTubeStream stream -> "youtube" .= stream
    in object $
    case status of
      Streaming {..} ->
        ["type" .= ("streaming" :: Text)
        ,serviceJSON _stream
        ,"title" .= _title
        ,"start_time" .= _startTime
        ,"viewer_count" .= _viewerCount]
      Hosting {..} ->
        ["type" .= ("hosting" :: Text)
        ,serviceJSON _stream
        ,"title" .= _title]
      Offline ->
        ["type" .= ("offline" :: Text)]



data ModEvent
  = DelayedMainChat ChatMessage
  | ModChat ChatMessage


instance ToJSON ModEvent where
  toJSON modEvent = case modEvent of
    DelayedMainChat chatMessage -> object $
      ["type" .= ("delayed_main_chat" :: Text)]
      ++ chatMessageJson chatMessage
    ModChat chatMessage -> object $
      ["type" .= ("mod_chat" :: Text)]
      ++ chatMessageJson chatMessage




data ChatMessage
  = AddUserMessage
      {user :: User
      ,userMessage :: UserMessage}
  | RemoveUserMessage
      {username :: Text
      ,userMessage :: UserMessage}
  | AddUser
      {user :: User}
  | RemoveUser
      {username :: Text}
  | CensorUser
     {username :: Text} -- removes all messages, ban or mute
  | SetUserList
      {userList :: HashMap Text User}



chatMessageJson chat = case chat of
    AddUserMessage {..} ->
      ["type_chat" .= ("add_user_message" :: Text)
      ,"user" .= simpleUserJSON user
      ,"time" .= _timestamp userMessage
      ,"message" .= _message userMessage
      ]
    RemoveUserMessage {..} ->
      ["type_chat" .= ("remove_user_message" :: Text)
      ,"username" .= username
      ,"message" .= _message userMessage
      ]
    AddUser {..} ->
      ["type_chat" .= ("add_user" :: Text)
      ,"user" .= simpleUserJSON user
      ]
    RemoveUser {..} ->
      ["type_chat" .= ("remove_user" :: Text)
      ,"username" .= username
      ]
    CensorUser {..} ->
      ["type_chat" .= ("censor_user" :: Text)
      ,"username" .= username
      ]
    SetUserList {..} ->
      ["type_chat" .= ("set_user_list" :: Text)
      ,"users" .= HashMap.map simpleUserJSON userList
      ]











data Notification
  = Whisper

instance ToJSON Notification where
  toJSON noti = object []




--------------------------------------------------------------------------------

data WSRequest
  = GESubscription GESub
  | GetStreamStatus
  | GetUsersInChat
  | UserReq UserRequest -- requets that require log in

data UserRequest
  = MainChatMessageRequest
      {message :: Text}
  | ModChatMessageRequest
      {message :: Text}
  | WhisperMessageRequest
      {receiver :: Text
      ,message :: Text}
  | ModerationRequest ModerationRequest -- must be mod

data ModerationRequest
  = UserInfo
  | ModAction ModAction


data ModAction
  = Censor Text Word -- username time length
  | Ban Text -- username



instance FromJSON WSRequest where
  parseJSON = withObject "WSRequest" $ \obj -> obj .: "type" >>= \case
    "gesub" -> fmap GESubscription $
      GESub <$> obj .: "main_chat"
            <*> obj .: "stream_status"
            <*> obj .: "mod"
    "get_stream_status" -> return GetStreamStatus
    "get_users_in_main_chat" -> return GetUsersInChat
    "message" -> fmap UserReq $ obj .: "type_chat" >>= \case
      "main" -> MainChatMessageRequest <$> obj .: "message"
      "mod" -> ModChatMessageRequest <$> obj .: "message"
      "whisper" -> WhisperMessageRequest <$> obj .: "receiver"
                                         <*> obj .: "message"
      str -> fail $ "Do Not Recognize Chat Type Request Type: " ++ str
    str -> fail $ "Do Not Recognize WS Request Type: " ++ str



--------------------------------------------------------------------------------
-- simple JSON
-- we have simple versions so mods & admins can look at the full info


simpleUserJSON :: User -> Value
simpleUserJSON (User {..}) = object
  ["username" .= _username
  ,"role" .= simpleRoleJSON _role
  ,"badges" .= toJSON
       (toList (_firstBadge _badges)
     ++ toList (_secondBadge _badges))
  ,"pronouns" .= _pronouns
  ,"name_color" .= nameColorJSON _role _nameColor
  ]


simpleRoleJSON :: Role -> Value
simpleRoleJSON (Right (SpecialRole roleName _)) = object ["special" .= roleName]
simpleRoleJSON (Left (Chatter 0 _)) = Null
simpleRoleJSON (Left (Chatter months sub)) = object
  ["months" .= months
  ,"tier" .= maybe 0 _subTier sub]


nameColorJSON :: Role -> NameColor -> Value
nameColorJSON role nameColor =
  let defaultColor = toJSON $ case _defaultNameColor nameColor of
        Left a -> a
        Right a -> a
      maybeLeft = maybe defaultColor toJSON $ _left nameColor
  in case role of
       Left (Chatter _ Nothing) -> defaultColor
       Left (Chatter _ (Just sub)) | _subTier sub == 1 -> defaultColor
       Left (Chatter _ (Just sub)) | _subTier sub == 2 -> toJSON maybeLeft
       _ -> case (_left nameColor, _right nameColor) of
         (Just left, Just right) -> object
           ["left" .= left
           ,"right" .= right
           ,"mode" .= _mode nameColor
           ]
         _ -> toJSON maybeLeft


makeLenses ''StreamStatus
