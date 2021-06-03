module Update.Page exposing (..)

import Browser.Dom
import Http
import Json.Decode as JD
import Json.Encode as JE
import Process
import Task
import Time exposing (Posix, Zone)

import Element exposing (Color)

import Accessors exposing (..)

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Update.ChatBox exposing (..)
import Update.ElmBar exposing (..)



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




updatePage : Update
updatePage model msg next = case (msg, model.page) of
  (HomePageMsg homePageMsg, HomePage homePageInfo) ->
    updateHomePageInfo model homePageMsg homePageInfo
  (TriggerAutoScrollDown, HomePage homePageInfo) -> pair model <| cmdMsg <|
    HomePageMsg <| HomeChatBoxMsg <| ChatBoxTriggerAutoScrollDown "homepage-"


  (ChatPageMsg chatPageMsg, ChatPage chatPageInfo) ->
    updateChatPageInfo model chatPageMsg chatPageInfo
  (TriggerAutoScrollDown, ChatPage chatPageInfo) -> pair model <| cmdMsg <|
    ChatPageMsg <| ChatPageChatBoxMsg <| ChatBoxTriggerAutoScrollDown "chatpage-"


  (ChatStreamPageMsg chatStreamPageMsg, ChatStreamPage chatStreamPageInfo) ->
    updateChatStreamPageInfo model chatStreamPageMsg chatStreamPageInfo
  (TriggerAutoScrollDown, ChatStreamPage chatStreamPageInfo) -> pair model <| cmdMsg <|
    ChatStreamPageMsg <| ChatStreamPageMessageRoomMsg <|
      GetElmBarViewport "chatstreampage-elmbar" <| AutoScrollDown False


  (StreamerPageMsg streamerPageMsg, StreamerPage streamerPageInfo) ->
    updateStreamerPageInfo model streamerPageMsg streamerPageInfo
  (TriggerAutoScrollDown, StreamerPage streamerPageInfo) -> pair model <| cmdMsg <|
    BatchMsgs
      [StreamerPageMsg <| StreamerPageChatBoxMsg <| ChatBoxTriggerAutoScrollDown "streamerpage-"
      ,StreamerPageMsg <| StreamerPageModRoomMsg <| MessageRoomMsg <|
        GetElmBarViewport "streamerpage-modchat-chatroom-messageroom-elmbar" (AutoScrollDown False)
      ,StreamerPageMsg <| StreamerPageMentionMessageRoomMsg <|
        GetElmBarViewport "streamerpage-mention-messageroom-elmbar" <| AutoScrollDown False
      ]

  _ -> next



updateHomePageInfo : UpdatePage HomePageMsg HomePageInfo
updateHomePageInfo model msg homePageInfo =
  let setHomePageInfo : HomePageInfo -> Model
      setHomePageInfo info = {model | page = case model.page of
        HomePage _ -> HomePage <| info
        anyOtherPage -> anyOtherPage}
  in case msg of
       HomeTestMsg subPage -> noCmd <| setHomePageInfo
         {homePageInfo | subPage = if isSubPageOn homePageInfo.subPage subPage
            then subPage else homePageInfo.subPage}
       HomeChatBoxMsg chatBoxMsg -> updateChatBox MainChat model chatBoxMsg homePageInfo.chatBox
         (HomePageMsg << HomeChatBoxMsg) <|
         \newChatBox -> setHomePageInfo <| {homePageInfo | chatBox = newChatBox}


       SetStreamScreenSize bool -> case (homePageInfo.streamScreenSize, bool) of
         -- From Expand To Shrink
         (True, False) ->
           pair (setHomePageInfo {homePageInfo | streamScreenSize = False})
             <| cmdMsg <| HomePageMsg <| InitiateSubPageSlide False
         -- From Shrink To Expand
         (False, True) ->
           pair (setHomePageInfo {homePageInfo | streamScreenSize = True})
             <| cmdMsg <| HomePageMsg <| SetSubPageDefault True
         _ -> noCmd <| setHomePageInfo {homePageInfo | streamScreenSize = bool}
       UpdateSubPage subPage -> noCmd <| setHomePageInfo
         {homePageInfo | subPage = if isSubPageOn homePageInfo.subPage subPage
            then subPage else homePageInfo.subPage}
       AppendSubPage subPage -> pair (setHomePageInfo
         {homePageInfo | streamScreenSize = False
                       , subPage = subPage
                       , history = homePageInfo.subPage::homePageInfo.history
                       , unappendHistory = Nothing})
         <| cmdMsg <| HomePageMsg <| InitiateSubPageSlide False
       UnappendSubPage -> pair (setHomePageInfo <|
         case homePageInfo.history of
           subPage :: history ->
             {homePageInfo | subPage = subPage
                           , history = history
                           , unappendHistory = Just homePageInfo.subPage
                           }
           _ -> homePageInfo)
         <| cmdMsg <| HomePageMsg <| InitiateSubPageSlide True
       SetSubPageDefault direction -> pair (setHomePageInfo
         {homePageInfo | subPage = HomeSubPageMain
                       , unappendHistory = Just homePageInfo.subPage
                       , history = []})
         <| cmdMsg <| HomePageMsg <| InitiateSubPageSlide direction
       InitiateSubPageSlide direction ->
         pair (setHomePageInfo {homePageInfo | initiateSubPageSliding = True
                                             , subPageSlideDirection = direction})
           <| do <| waitAFrame <| Task.succeed <| HomePageMsg TurnOffSubPageSlide
       TurnOffSubPageSlide -> noCmd <| setHomePageInfo {homePageInfo | initiateSubPageSliding = False}
       SlideSubPageHeader bool direction subPage -> if bool
         then pair (setHomePageInfo {homePageInfo
                | subPage = subPage
                , initiateSubPageHeaderSliding = True
                , subPageHeaderSlideDirection = direction})
              <| do <| waitAFrame <| Task.succeed <| HomePageMsg
                    <| SlideSubPageHeader False direction subPage
         else noCmd <| setHomePageInfo
                {homePageInfo | initiateSubPageHeaderSliding = False}


       -- NavBar
       CheckStreamTitleLength -> pair model
         <| flip Task.attempt (Browser.Dom.getElement "stream-title")
            <| \resultElement ->
                 case resultElement of
                      Err _ -> HomePageMsg <| ChangeStreamTitleLength 0
                      Ok element -> HomePageMsg <| ChangeStreamTitleLength
                        element.element.width



       ChangeStreamTitleLength float -> noCmd <| setHomePageInfo
         {homePageInfo | streamTitleLength = float}
       HoverTitle bool -> noCmd <| setHomePageInfo
         {homePageInfo | hoverTitle = bool}
       NextCounterPosition -> noCmd <| setHomePageInfo
         {homePageInfo | counterPosition = modBy 2 <| homePageInfo.counterPosition + 1}


       -- Subscribe
       GiftFriendCheck strCheck -> pair model <|
         case homePageInfo.subPage of
           HomeSubPageSubscribe info -> if info.giftFriendSearch == strCheck
             then do <| Task.succeed <| HomePageMsg <| POSTGiftFriendCheck strCheck
             else Cmd.none
           _ -> Cmd.none
       POSTGiftFriendCheck strCheck -> pair model <| cmdMsg <|
         ApiPOST [] "subscribe/check" "check_username"
           [pair "username" <| JE.string strCheck]
           (JD.map (HomePageMsg << UpdateGiftFriendCheck strCheck)
                   (JD.field "username" JD.int))
           <| defaultRequestResponse
       {- this message could exist in POSTGiftFriendCheck, but if the response takes
          a long time, it could break the page by resetting the model -}
       UpdateGiftFriendCheck strCheck int -> noCmd <| setHomePageInfo
         {homePageInfo | subPage =
            case homePageInfo.subPage of
              HomeSubPageSubscribe info -> HomeSubPageSubscribe <|
                if info.giftFriendSearch == strCheck
                   then {info | inValidGiftFriendSearch = int} else info
              _ -> homePageInfo.subPage}
       POSTSubscription info -> pair model <| cmdMsg <|
         let (subscribeType, subDataType, subJson) = case info.recipient of
               Positive -> ("subscribe_self", "auto_renew", JE.bool info.autoRenew)
               Neutral -> ("subscribe_friend", "username", JE.string info.giftFriendSearch)
               Negative -> ("subscribe_random", "num_of_gifts", JE.int info.numberOfRandomGiftSubs)
         in ApiPOST [] "subscribe" subscribeType
              [pair "tier" <| JE.int info.tier
              ,pair "3_month_package" <| JE.bool info.month3Package
              ,pair "message" <| JE.string info.message
              ,pair subDataType subJson]
              (JD.succeed NoMsg)
              <| defaultRequestResponse
       POSTDonation info -> case String.toInt info.amount of
         Nothing -> noCmd model
         Just amount -> pair model <| cmdMsg <|
           ApiPOST [] "subscribe" "donation"
             [pair "name" <| JE.string info.name
             ,pair "amount" <| JE.int amount
             ,pair "message" <| JE.string info.message]
             (JD.succeed NoMsg)
             <| defaultRequestResponse


--------------------------------------------------------------------------------



--homePageInfoSetUp =
--  [do <| Task.map (\posix -> UpdateLiveTime <| Time.posixToMillis posix // 1000)
--                  Time.now
--  --,cmdMsg <| liftStreamInfoMsg GETStreamStatus
--  ]
--
--
--
--
--homePageInfoSubBatch model =
--  [case model.homePageInfo.streamStatus of
--     Just (Streaming _) -> Time.every 1000 <| \posix ->
--       UpdateLiveTime <| Time.posixToMillis posix // 1000
--     _-> Sub.none
--  ]





updateChatPageInfo : UpdatePage ChatPageMsg ChatPageInfo
updateChatPageInfo model msg chatPageInfo =
  let setChatPageInfo info = {model | page = ChatPage info}
  in case msg of
       ChatPageChatBoxMsg chatBoxMsg -> updateChatBox MainChat model chatBoxMsg chatPageInfo.chatBox
         (ChatPageMsg << ChatPageChatBoxMsg) <|
         \newChatBox -> setChatPageInfo <| {chatPageInfo | chatBox = newChatBox}



updateChatStreamPageInfo : UpdatePage ChatStreamPageMsg ChatStreamPageInfo
updateChatStreamPageInfo model msg chatStreamPageInfo =
  let setChatStreamPageInfo info = {model | page = ChatStreamPage info}
  in case msg of
       ChatStreamPageMessageRoomMsg elmBarMsg -> updateElmBar updateMessageRoom model
         elmBarMsg chatStreamPageInfo.messageRoom
         (ChatStreamPageMsg << ChatStreamPageMessageRoomMsg)
         <| \newMessageRoom -> setChatStreamPageInfo {chatStreamPageInfo | messageRoom = newMessageRoom}
      -- ChatStreamPageMessageRoomMsg mRoomMsg -> updateMessageRoom model mRoomMsg chatStreamPageInfo.messageRoom
      --   (ChatStreamPageMsg << ElmBarMsg << ChatStreamPageMessageRoomMsg) <|
      --   \newMRoom -> setChatStreamPageInfo <| {chatStreamPageInfo | messageRoom = newMRoom}
--





updateStreamerPageInfo : UpdatePage StreamerPageMsg StreamerPageInfo
updateStreamerPageInfo model msg streamerPageInfo =
  let setStreamerPageInfo : StreamerPageInfo -> Model
      setStreamerPageInfo info = {model | page = case model.page of
        StreamerPage _ -> StreamerPage <| info
        anyOtherPage -> anyOtherPage}
  in case msg of
       StreamerPageChatBoxMsg chatBoxMsg -> updateChatBox MainChat model chatBoxMsg streamerPageInfo.chatBox
         (StreamerPageMsg << StreamerPageChatBoxMsg) <|
         \newChatBox -> setStreamerPageInfo <| {streamerPageInfo | chatBox = newChatBox}
       StreamerPageModRoomMsg chatRoomMsg -> updateChatRoom ModChat model chatRoomMsg
         streamerPageInfo.modRoom (StreamerPageMsg << StreamerPageModRoomMsg)
         <| \newChatRoom -> setStreamerPageInfo {streamerPageInfo | modRoom = newChatRoom}
       StreamerPageMentionMessageRoomMsg elmBarMsg -> updateElmBar updateMessageRoom model
         elmBarMsg streamerPageInfo.atMessageRoom
         (StreamerPageMsg << StreamerPageMentionMessageRoomMsg)
         <| \newMessageRoom -> setStreamerPageInfo {streamerPageInfo | atMessageRoom = newMessageRoom}

       OverStreamerPage f -> noCmd <| setStreamerPageInfo <| f streamerPageInfo
       StreamStatusSetter status -> noCmd <| setStreamerPageInfo {streamerPageInfo | streamStatus = status}
       HostCheck strCheck -> pair model <| cmdMsg <|
         ApiPOST [] "mod" "host_check"
           [pair "name" <| JE.string strCheck]
           (flip JD.map (JD.field "check_result" JD.int) <| \int ->
             StreamerPageMsg <| OverStreamerPage <| \info ->
                  if info.hostingSearch /= strCheck then info
                     else  {info | hostingCheck = int})
           <| defaultRequestResponse
