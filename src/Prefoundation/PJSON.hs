module Prefoundation.PJSON where

import qualified Data.HashMap.Strict as HashMap

import Import.NoFoundation

import Prefoundation.User
import Prefoundation.WS

type PValue = Int -> Value

class ToPermittedJSON a where
  toPJSON_ :: a -> STM (Int -> [(Text,Value)])
  --toPJSON_ :: MonadIO m => a -> m (Int -> [(Text,Value)])

toPJSON :: ToPermittedJSON a => a -> STM PValue
toPJSON v = object <$$> toPJSON_ v

toPJSON' :: ToPermittedJSON a => a -> STM Value
toPJSON' v = toPJSON_ v <&> \f -> object (f 2)



instance ToPermittedJSON GlobalEvent where
  toPJSON_ globalEvent = case globalEvent of
    MainChatGlobalEvent chatMessage -> do
      toMessageJSON <- toPJSON_ chatMessage
      return $ \permissions ->
        ("type" .= ("main_chat" :: Text)) : toMessageJSON permissions
    StreamStatusGlobalEvent streamStatus -> 
      let serviceJSON stream = case stream of
            TwitchStream stream -> "twitch" .= stream
            YouTubeStream stream -> "youtube" .= stream
      in return $ \_ -> case streamStatus of
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
    ModGlobalEvent modEvent -> toPJSON_ modEvent



instance ToPermittedJSON ModEvent where
  toPJSON_ modEvent = case modEvent of
    AddDelayedUserMessage user userMessage -> do
      toUserJSON <- toPJSON user
      return $ \permissions ->
        ["type" .= ("main_chat" :: Text)
        ,"type_chat" .= ("add_delayed_user_message" :: Text)
        ,"user" .= toUserJSON permissions
        ,"time" .= _timestamp userMessage
        ,"render_links" .= (isRight (_role user) || isSafeUser (_moderation user))
        ,"message" .= _message userMessage
        ]
    ModChat chatMessage -> do
      chatMessageJSON <- toPJSON_ chatMessage
      return $ \permissions ->
        ("type" .= ("mod_chat" :: Text)) : chatMessageJSON permissions


instance ToPermittedJSON User where
  toPJSON_ User{..} = do
    toRoleJSON <- toPJSON _role
    return $ \permissions ->
      ["username" .= _username
      ,"role" .= toRoleJSON permissions
      ,"badges" .= toJSON
           (toList (_firstBadge _badges)
         ++ toList (_secondBadge _badges))
      ,"pronouns" .= _pronouns
      ,"name_color" .= nameColorJSON _role _nameColor
      ] ++ whem (permissions >= 1)
      []


instance ToPermittedJSON Role where
  toPJSON_ (Right r) = readTVar r <&> \SpecialRole{..} -> \permissions ->
    ["special" .= _roleName
    ,"order" .= _order]
  --toPJSON_ permissions (Left (Chatter 0 _)) = return []
  toPJSON_ (Left Chatter{..}) = return $ \permissions ->
    ["months" .= _months
    ,"tier" .= maybe 0 _subTier _subscription]


instance ToPermittedJSON ChatEvent where
  toPJSON_ chatEvent = case chatEvent of
    AddUserMessage user userMessage -> do
      toUserJSON <- toPJSON user
      return $ \permissions ->
        ["type_chat" .= ("add_user_message" :: Text)
        ,"user" .= toUserJSON permissions
        ,"time" .= _timestamp userMessage
        ,"render_links" .= (isRight (_role user) || isSafeUser (_moderation user))
        ,"message" .= _message userMessage
        ]
    RemoveUserMessage{..} ->
      return $ \permissions ->
        ["type_chat" .= ("remove_user_message" :: Text)
        ,"username" .= userUsername
        ,"message" .= _message userMessage
        ]
    AddUser user -> do
      toUserJSON <- toPJSON user
      return $ \permissions ->
        ["type_chat" .= ("add_user" :: Text)
        ,"user" .= toUserJSON permissions
        ]
    RemoveUser{..} ->
      return $ \permissions ->
        ["type_chat" .= ("remove_user" :: Text)
        ,"username" .= userUsername
        ]
    MuteUser{..} ->
      return $ \permissions ->
        ["type_chat" .= ("mute_user" :: Text)
        ,"username" .= userUsername
        ]
    SetUserList{..} -> do
      userJSONList <- traverse toPJSON userList
      return $ \permissions ->
        ["type_chat" .= ("set_user_list" :: Text)
        ,"users" .= fmap ($ permissions) userJSONList
        ]



instance ToPermittedJSON StreamStatus where
  toPJSON_ status =
    let serviceJSON stream = case stream of
          TwitchStream stream -> "twitch" .= stream
          YouTubeStream stream -> "youtube" .= stream
    in return $ case status of
         Streaming {..} -> \_ ->
           ["status" .= ("streaming" :: Text)
           ,serviceJSON _stream
           ,"title" .= _title
           ,"start_time" .= _startTime
           ,"viewer_count" .= _viewerCount]
         Hosting {..} -> \_ ->
           ["status" .= ("hosting" :: Text)
           ,serviceJSON _stream
           ,"title" .= _title]
         Offline -> \_ ->
           ["status" .= ("offline" :: Text)]




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
