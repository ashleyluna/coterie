module Update.LiveInfo exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Process
import Task
import Time exposing (Posix, Zone)

import Element exposing (Color)

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Update.Chat exposing (..)
import Streamer exposing (..)



--type alias HasStreamInfo model =
--  {streamInfoLens : Lens model StreamInfo}
----type alias HasStreamInfo model msg =
----  {streamInfoLens : Lens model (StreamInfo msg)}
--
--
--
--
--type alias StreamInfo = -- argument = msg
--  { --liftStreamInfoMsg : StreamInfoMsg -> msg
--
--  --,
--  streamStatus : Maybe StreamStatus
--   -- Community
--  ,usersInChatList : List String -- TODO ChatUser
--  ,subGiftersLeaderBoard : List String
--  }
--
--
--type StreamingService = TwitchStream String
--                      | YouTubeStream String
--
--
--type StreamStatus = Streaming StreamingRecord
--                  | Hosting HostingRecord
--                  | Offline
--
--type alias StreamingRecord =
--  {stream : StreamingService
--  ,title : String
--  ,liveTime : {start : Int -- start = stream started at 2 pm, Int = number of seconds in Posix
--              ,upTime : Maybe Int}
--  ,viewerCount : Int} -- current time is 3pm, so upTime = 1 hour
--
--type alias HostingRecord =
--  {stream : StreamingService
--  ,title : String}

--------------------------------------------------------------------------------




updateLiveInfo : Update
updateLiveInfo model msg next =
  let setLiveInfo info = {model | liveInfo = info}
      overLiveInfo f = {model | liveInfo = f model.liveInfo}
      overStreamStatus f = overLiveInfo <| \liveInfo ->
        {liveInfo | streamStatus = f liveInfo.streamStatus}
  in case msg of
       UpdateStreamStatus newStatus ->
          (overStreamStatus <| \streamStatus -> Just <| case (newStatus, streamStatus) of
             (Streaming newRecord, Just (Streaming oldRecord)) -> Streaming <|
               {newRecord | upTime = oldRecord.upTime}
             _ -> newStatus
          ,cmdMsg <| BatchMsgs
             [HomePageMsg CheckStreamTitleLength
             ,let sendSystemMessage = MainChatMsg << AddSystemMessage MainChatMsg
              in case (newStatus, model.liveInfo.streamStatus) of
                 (Streaming _, Nothing) -> NoMsg
                 (Streaming _, Just (Streaming _)) -> NoMsg
                 (Streaming _, _) -> sendSystemMessage <|
                   streamerInfo.name ++ " will begin shortly"
                 (Hosting hostee, _) -> sendSystemMessage <|
                   "Hosting " ++ hostee.title
                 (Offline, Just Offline) -> NoMsg -- TODO idk if this will ever fire, remove?
                 (Offline, _) -> BatchMsgs
                   [sendSystemMessage <|
                      streamerInfo.name ++ (if streamerInfo.plural then " are " else " is ")
                                        ++ "offline"
                   ,case model.page of
                      HomePage info -> if info.streamScreenSize
                        then HomePageMsg <| SetStreamScreenSize False
                        else NoMsg
                      _ -> NoMsg
                   ]
             ])
       ChangeStreamStatus newStatus -> pair model <| cmdMsg <|
         ApiPOST [] "mod"
           (case newStatus of
             Just (Ok _) -> "change_title"
             Just (Err _) -> "host"
             Nothing -> "go_offline"
           )
           (case newStatus of
             Just (Ok str) -> [pair "title" <| JE.string str]
             Just (Err str) -> [pair "creator" <| JE.string str]
             Nothing -> []
           )
           (JD.succeed NoMsg)
           <| defaultRequestResponse
       UpdateLiveTime currentTime -> noCmd <| overStreamStatus <| \streamStatus ->
         case streamStatus of
           Just (Streaming info) -> Just <|
             Streaming {info | startTime = info.startTime
                             , upTime = Just <| currentTime - info.startTime}
           x -> x


       -- Main Chat
       MainChatMsg chatMsg -> updateChat model chatMsg model.liveInfo.mainChat MainChatMsg
         <| \newChat -> overLiveInfo <| \liveInfo -> {liveInfo | mainChat = newChat}
       ModChatMsg chatMsg -> updateChat model chatMsg model.liveInfo.modChat ModChatMsg
         <| \newChat -> overLiveInfo <| \liveInfo -> {liveInfo | modChat = newChat}
       _ -> next

--------------------------------------------------------------------------------



liveInfoSetUp =
  [do <| Task.map (\posix -> UpdateLiveTime <| Time.posixToMillis posix // 1000)
                  Time.now
  --,cmdMsg <| liftStreamInfoMsg GETStreamStatus
  ]




liveInfoSubBatch model =
  [case model.liveInfo.streamStatus of
     Just (Streaming _) -> Time.every 1000 <| \posix ->
       UpdateLiveTime <| Time.posixToMillis posix // 1000
     _-> Sub.none
  ]




--------------------------------------------------------------------------------




isStreamOffline model = model.liveInfo.streamStatus == Just Offline
