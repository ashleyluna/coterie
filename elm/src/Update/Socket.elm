module Update.Socket exposing (..)

import Dict
import Http
import Json.Decode as JD
import Json.Encode as JE
import String.Extra as StringE

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)







socketUpdates : Update
socketUpdates model msg next = case msg of
  SocketRequest req -> pair model <| socketMessageEncoder req model
  SocketResponse str ->
    let decoder = jdFieldType "type" <| socketMessageDecoder model
    in case JD.decodeString decoder str of
         Ok cmd -> pair model <| cmdMsg cmd
         Err err -> pair model <| cmdMsg <| LogMessage <| JD.errorToString err
  _ -> next


socketMessageEncoder : SocketRequest -> Model -> Cmd Msg
socketMessageEncoder req model =
  let send str vals = [pair "type" <| JE.string str] ++ vals
      --localeName locale = case locale of
      --  MainChat -> [pair "type_chat" <| JE.string "main"]
      --  ModChat -> [pair "type_chat" <| JE.string "mod"]
      --  Whisper receiver -> [pair "type_chat" <| JE.string "whisper"
      --                      ,pair "receiver" <| JE.string receiver]
  in sendOverSocket <| JE.encode 0 <| JE.object <| case req of
       GESubscription sub -> send "gesub" <|
         [pair "main_chat" <| JE.bool sub.mainChat
         ,pair "stream_status" <| JE.bool sub.streamStatus
         ,pair "mod" <| JE.bool sub.mod
         ]
       GetStreamStatus -> send "get_stream_status" []
       GetUsersInChat -> send "get_users_in_main_chat" []
       --UserMessageRequest locale str -> send "message" <|
       --  [pair "message" <| JE.string <|
       --    String.trim <| StringE.clean str
       --  ] ++ localeName locale



socketMessageDecoder : Model -> String -> JD.Decoder Msg
socketMessageDecoder model response = case response of
  "streaming" -> JD.map (UpdateStreamStatus << Streaming) <| JD.map5 StreamingRecord
    (JD.oneOf [JD.map TwitchStream <| JD.field "twitch" JD.string
              ,JD.map YouTubeStream <| JD.field "youtube" JD.string])
    (JD.field "title" JD.string)
    (JD.field "start_time" JD.int)
    (JD.succeed Nothing) -- upTime,
    (JD.field "viewer_count" JD.int)
  "hosting" -> JD.map (UpdateStreamStatus << Hosting) <| JD.map2 HostingRecord
    (JD.oneOf [JD.map TwitchStream <| JD.field "youtube" JD.string
              ,JD.map YouTubeStream <| JD.field "twitch" JD.string])
    (JD.field "title" JD.string)
  "offline" -> JD.succeed <| UpdateStreamStatus Offline
  "main_chat" -> JD.map MainChatMsg <| jdFieldType "type_chat" <|
    socketChatMessageDecoder model.commonInfo model.liveInfo.mainChat.users
  "mod_chat" -> JD.map ModChatMsg <| jdFieldType "type_chat" <|
    socketChatMessageDecoder model.commonInfo model.liveInfo.mainChat.users -- TODO modChat
  _ -> JD.fail <| "Do not recognise socket response type: '" ++ response ++ "'"




socketChatMessageDecoder : CommonInfo -> ChatUserList -> String -> JD.Decoder ChatMsg
socketChatMessageDecoder commonInfo users response =
  let jdUserMessage =
        JD.map4 (\user time renderLinks -> UserMessageRecord user
          (microToMillis time) <<
          parseMessage commonInfo users
            (RenderOptions (user.role == Nothing) renderLinks))
        (JD.field "user" jdUser)
        (JD.field "time" JD.int)
        (JD.field "render_links" JD.bool)
        (JD.field "message" JD.string)
  in case response of
  "add_user_message" -> JD.map (AddChatMessage << UserMessage) jdUserMessage
  "add_delayed_user_message" -> JD.map AddDelatedUserMessage jdUserMessage
  "remove_user_message" -> JD.map2 RemoveUserMessage
    (JD.field "username" JD.string)
    (JD.field "message" JD.string)
  "add_user" -> JD.map AddChatUser
    (JD.field "user" jdUser)
  "remove_user" -> JD.map RemoveChatUser
    (JD.field "username" JD.string)
  "censor_user" -> JD.map CensorChatUser
    (JD.field "username" JD.string)
  "set_user_list" -> JD.map SetUserList
    (JD.field "users" <| JD.dict jdUser)
  _ -> JD.fail <| "Do not recognise type_chat: '" ++ response ++ "'"


jdUser = JD.map5 ChatUser
  (JD.field "username" JD.string)
  (JD.maybe <| JD.field "role" jdRole)
  (JD.field "badges" <| JD.list JD.string)
  (JD.maybe <| JD.field "pronouns" JD.string)
  (JD.field "name_color" jdNameColor_)
