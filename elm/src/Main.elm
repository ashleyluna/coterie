module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation exposing (Key, replaceUrl)
import Dict
import Task
import Time exposing (Posix, Zone)
import Url exposing (Url)

import Json.Decode as JD
import Json.Encode as JE

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Page.Home exposing (..)
import Page.Streamer exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)
import Main.Update exposing (update)
import Main.View exposing (view)
import Streamer exposing (..)
import Update.Chat exposing (..)
import Update.CommonInfo exposing (..)
import Update.LiveInfo exposing (..)




main = Browser.application
  {init = init
  ,view = view
  ,update = update
  ,subscriptions = initSubscriptions
  ,onUrlRequest = RequestUrl --: UrlRequest -> msg
  ,onUrlChange = ChangeUrl --: Url -> msg
  }


init : JD.Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let --parentHost = "http://localhost:3000"
      jsonFlag flagName decoder defaultInfo defaultMsg =
        case JD.decodeValue decoder flags of
          Ok info -> (info, cmdMsg <| LogMessage <| "Valid " ++ flagName)
          Err err -> (defaultInfo
                     ,Cmd.batch [logMessage <| "Invalid " ++ flagName ++ ": " ++ JD.errorToString err
                                ,defaultMsg])
      (cookie, cookieCmd) = jsonFlag "Cookie"
        (JD.field "cookie" JD.string) "" Cmd.none
      (staticInfo, staticInfoCmd) = jsonFlag "StaticInfo"
        staticInfoFlagDecoder defaultStaticInfo Cmd.none
      (settings, settingsCmd) = jsonFlag "Settings"
        settingsFlagDecoder defaultMainUserSettings <|
        setSettingsStorage <| encodeMainUserSettings
                              defaultMainUserSettings
      initialState page initPageCmds =
        ({setUp = {profile = False
                  ,liveTime = False}
         ,commonInfo =
           {localInfo =
             {url = url
             ,key = key
             ,zone = Nothing
             ,cookie = cookie
             }
           ,staticInfo = staticInfo
           ,profile = Nothing
           ,settings = settings
           }
         ,liveInfo = defaultLiveInfo
         ,page = page}
        ,Cmd.batch
           <| [cookieCmd
              ,staticInfoCmd
              ,settingsCmd]
           ++ liveInfoSetUp
           ++ chatSetUp
           ++ commonInfoSetUp
           ++ initPageCmds
           ++ [do <| Task.map SetZone Time.here
              ,cmdMsg <| LogMessage "09:12"
              ,cmdMsg <| LogMessage <| apiUrl url
              ])
      stdInitialGE =
        [cmdMsg <| SocketRequest <| GESubscription
          {mainChat = True
          ,streamStatus = True
          ,mod = True
          }
        ,cmdMsg <| SocketRequest <| GetStreamStatus
        ,cmdMsg <| SocketRequest <| GetUsersInChat
        ]
  in case url.path of
       "/chat" -> initialState initialChatPageInfo <|
         stdInitialGE
       "/chat/stream" -> initialState initialChatStreamPageInfo <|
         [cmdMsg <| SocketRequest <| GESubscription
           {mainChat = True
           ,streamStatus = False
           ,mod = False
           }
         ,cmdMsg <| SocketRequest <| GetUsersInChat
         ]
       "/streamer" -> initialState initialStreamerPageInfo <|
         stdInitialGE
       --"mod" ->
       --"admin" ->

       -- home
       _ -> initialState initialHomePageInfo <|
         [cmdMsg <| HomePageMsg CheckStreamTitleLength
         ,case url.path of
           "" -> Cmd.none
           _ -> replaceUrl key <| Url.toString {url | path = "/"}]
         ++
         stdInitialGE

initSubscriptions : Model -> Sub Msg
initSubscriptions model = Sub.batch
  <| liveInfoSubBatch model
  ++ commonInfoSubBatch model
  ++
  [receiveCookie ReceiveCookie
  ,receiveOverSocket SocketResponse
  ,postGoogleSignIn POSTGoogleSignIn
  ]



staticInfoFlagDecoder =
  JD.field "static_info" <| JD.map7 StaticInfo
    (JD.field "special_roles" <| JD.dict JD.int) -- left == top
    (JD.field "special_role_badges" jdDictEmote)
    (JD.field "sub_badges" <| JD.array <| JD.list <| JD.map2 SubscriptionBadge
      (JD.field "months_required" JD.int)
      (JD.field "emote" jdEmote))
    (JD.field "season_badges" <| JD.list jdEmote)
    (JD.field "common_badges" <| JD.dict jdEmote)
    (JD.field "global_emote_list" jdDictEmote)
    (JD.field "sub_only_emote_list" jdDictEmote)
    --(JD.field "pointsRewards" <| JD.dict JD.int)
     --JD.list <| JD.map2 PointsReward
      --(JD.field "name" JD.string)
      --(JD.field "cost" JD.string))


settingsFlagDecoder =
  JD.field "settings" <| JD.map5 identity (JD.map8 SettingsRecord
   (JD.field "theme_mode" JD.bool)
   (JD.field "show_timestamps" JD.bool)
   (JD.field "show_emotes" JD.bool)
   (JD.field "animate_emotes" JD.bool)
   (JD.field "show_badges" JD.bool)
   (JD.field "style_usernames" <| JD.nullable JD.bool)
   (JD.field "text_emphasis" <| JD.nullable JD.bool)
   (JD.field "text_size" JD.int))

   (JD.field "mention_ignored_users" JD.bool)
   (JD.field "curse_swear_words" JD.bool)
   (JD.field "nsfw" JD.bool)
   (JD.field "show_banned_messages" <| JD.nullable JD.bool)

defaultStaticInfo =
  {specialRoles = Dict.fromList []
  ,specialRoleBadges = Dict.fromList []
  ,subBadges = Array.fromList []
  ,seasonBadges = []
  ,badges = Dict.fromList []
  ,globalEmoteList = Dict.fromList []
  ,subOnlyEmoteList = Dict.fromList []
  --,pointsRewards = Dict.fromList []
  }

defaultLiveInfo =
  {streamStatus = Nothing
  --,shoutOutBox =
  ,mainChat =
    {users = []
    ,messages = Dict.fromList []}
  ,whispers = Nothing
  ,modChat =
    {users = {specialUsers = []
             ,chatters = Dict.fromList []}
    ,messages = []}
  --,subGiftersLeaderBoard = []
  }


defaultMainUserSettings =
  {themeMode = False
  ,textEmphasis = Just True
  ,textSize = 16
  ,showTimestamps = True
  ,showBadges = True
  ,showEmotes = True
  ,animateEmotes = True
  ,styleUsernames = Just True
  ,mentionIgnoredUsers = False
  ,curseSwearWords = True
  ,nsfw = True
  ,showBannedMessages = Just True}
