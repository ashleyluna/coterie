module Update.Page exposing (..)

import Browser.Dom
import Http
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




--updatePage : Update
updatePage model msg next = case (msg, model.page) of
  (HomePageMsg homePageMsg, HomePage homePageInfo) ->
    updateHomePageInfo model homePageMsg homePageInfo
  (TriggerAutoScrollDown, HomePage homePageInfo) -> pair model <| cmdMsg <|
    Msg <| HomePageMsg <| HomeChatBoxMsg <| Msg <| ChatBoxTriggerAutoScrollDown "homepage-"


  (ChatPageMsg chatPageMsg, ChatPage chatPageInfo) ->
    updateChatPageInfo model chatPageMsg chatPageInfo
  (TriggerAutoScrollDown, ChatPage chatPageInfo) -> pair model <| cmdMsg <|
    Msg <| ChatPageMsg <| ChatPageChatBoxMsg <| Msg <| ChatBoxTriggerAutoScrollDown "chatpage-"


  (ChatStreamPageMsg chatStreamPageMsg, ChatStreamPage chatStreamPageInfo) ->
    updateChatStreamPageInfo model chatStreamPageMsg chatStreamPageInfo
  (TriggerAutoScrollDown, ChatStreamPage chatStreamPageInfo) -> pair model <| cmdMsg <|
    Msg <| ChatStreamPageMsg <| ChatStreamPageMessageBoxMsg <| Msg <|
      getViewportAndAutoScroll MessageBoxElmBarMsg "chatstreampage-elmbar"


  (StreamerPageMsg streamerPageMsg, StreamerPage streamerPageInfo) ->
    updateStreamerPageInfo model streamerPageMsg streamerPageInfo
  (TriggerAutoScrollDown, StreamerPage streamerPageInfo) -> pair model <| cmdMsg <|
    BatchMsgs
      [Msg <| StreamerPageMsg <| StreamerPageChatBoxMsg <| Msg <| ChatBoxTriggerAutoScrollDown "streamerpage-"
      ,Msg <| StreamerPageMsg <| StreamerPageModRoomMsg <| Msg <| MessageBoxMsg <| Msg <|
        getViewportAndAutoScroll MessageBoxElmBarMsg "streamerpage-modchat-chatroom-messageroom-elmbar"
      ,Msg <| StreamerPageMsg <| StreamerPageMentionMessageBoxMsg <| Msg <|
        getViewportAndAutoScroll MessageBoxElmBarMsg "streamerpage-mention-messageroom-elmbar"
      ]

  _ -> next



--updateHomePageInfo : UpdatePage HomePageMsg HomePageInfo
updateHomePageInfo model msg homePageInfo =
  let setHomePageInfo : HomePageInfo -> Model
      setHomePageInfo info = {model | page = case model.page of
        HomePage _ -> HomePage <| info
        anyOtherPage -> anyOtherPage}
  in case msg of
       HomeTestMsg subPage -> noCmd <| setHomePageInfo
         {homePageInfo | subPage = if isSubPageOn homePageInfo.subPage subPage
            then subPage else homePageInfo.subPage}
       HomeChatBoxMsg chatBoxMsg -> updateChatBox
         homePageInfo.chatBox (\newChatBox -> setHomePageInfo <| {homePageInfo | chatBox = newChatBox})
         model chatBoxMsg (Msg << HomePageMsg << HomeChatBoxMsg)


       SetStreamScreenSize bool -> case (homePageInfo.streamScreenSize, bool) of
         -- From Expand To Shrink
         (True, False) ->
           pair (setHomePageInfo {homePageInfo | streamScreenSize = False})
             <| cmdMsg <| Msg <| HomePageMsg <| InitiateSubPageSlide False
         -- From Shrink To Expand
         (False, True) ->
           pair (setHomePageInfo {homePageInfo | streamScreenSize = True})
             <| cmdMsg <| Msg <| HomePageMsg <| SetSubPageDefault True
         _ -> noCmd <| setHomePageInfo {homePageInfo | streamScreenSize = bool}
       UpdateSubPage subPage -> noCmd <| setHomePageInfo
         {homePageInfo | subPage = if isSubPageOn homePageInfo.subPage subPage
            then subPage else homePageInfo.subPage}
       AppendSubPage subPage -> pair (setHomePageInfo
         {homePageInfo | streamScreenSize = False
                       , subPage = subPage
                       , history = homePageInfo.subPage::homePageInfo.history
                       , unappendHistory = Nothing})
         <| cmdMsg <| Msg <| HomePageMsg <| InitiateSubPageSlide False
       UnappendSubPage -> pair (setHomePageInfo <|
         case homePageInfo.history of
           subPage :: history ->
             {homePageInfo | subPage = subPage
                           , history = history
                           , unappendHistory = Just homePageInfo.subPage
                           }
           _ -> homePageInfo)
         <| cmdMsg <| Msg <| HomePageMsg <| InitiateSubPageSlide True
       SetSubPageDefault direction -> pair (setHomePageInfo
         {homePageInfo | subPage = MainMainBoxPage
                       , unappendHistory = Just homePageInfo.subPage
                       , history = []})
         <| cmdMsg <| Msg <| HomePageMsg <| InitiateSubPageSlide direction
       InitiateSubPageSlide direction ->
         pair (setHomePageInfo {homePageInfo | initiateSubPageSliding = True
                                             , subPageSlideDirection = direction})
           <| do <| waitAFrame <| Task.succeed <| Msg <| HomePageMsg TurnOffSubPageSlide
       TurnOffSubPageSlide -> noCmd <| setHomePageInfo {homePageInfo | initiateSubPageSliding = False}
       SlideSubPageHeader bool direction subPage -> if bool
         then pair (setHomePageInfo {homePageInfo
                | subPage = subPage
                , initiateSubPageHeaderSliding = True
                , subPageHeaderSlideDirection = direction})
              <| do <| waitAFrame <| Task.succeed <| Msg <| HomePageMsg
                    <| SlideSubPageHeader False direction subPage
         else noCmd <| setHomePageInfo
                {homePageInfo | initiateSubPageHeaderSliding = False}


       -- NavBar
       CheckStreamTitleLength -> pair model
         <| flip Task.attempt (Browser.Dom.getElement "stream-title")
            <| \resultElement -> Msg <| case resultElement of
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
             then do <| Task.succeed <| Msg <| HomePageMsg <| POSTGiftFriendCheck strCheck
             else Cmd.none
           _ -> Cmd.none
       POSTGiftFriendCheck strCheck -> pair model <|
         apiPOSTDefault "subscribe/check" "check_username"
           [pair "username" <| JE.string strCheck] <|
           JD.map (Msg << HomePageMsg << UpdateGiftFriendCheck strCheck)
                   (JD.field "username" JD.int)
       {- this message could exist in POSTGiftFriendCheck, but if the response takes
          a long time, it could break the page by resetting the model -}
       UpdateGiftFriendCheck strCheck int -> noCmd <| setHomePageInfo
         {homePageInfo | subPage =
            case homePageInfo.subPage of
              HomeSubPageSubscribe info -> HomeSubPageSubscribe <|
                if info.giftFriendSearch == strCheck
                   then {info | inValidGiftFriendSearch = int} else info
              _ -> homePageInfo.subPage}
       POSTSubscription info -> pair model <|
         let (subscribeType, subDataType, subJson) = case info.recipient of
               Positive -> ("subscribe_self", "auto_renew", JE.bool info.autoRenew)
               Neutral -> ("subscribe_friend", "username", JE.string info.giftFriendSearch)
               Negative -> ("subscribe_random", "num_of_gifts", JE.int info.numberOfRandomGiftSubs)
         in apiPOSTDefault "subscribe" subscribeType
              [pair "tier" <| JE.int info.tier
              ,pair "3_month_package" <| JE.bool info.month3Package
              ,pair "message" <| JE.string info.message
              ,pair subDataType subJson] <|
              JD.succeed NoMsg
       POSTDonation info -> case String.toInt info.amount of
         Nothing -> noCmd model
         Just amount -> pair model <|
           apiPOSTDefault "subscribe" "donation"
             [pair "name" <| JE.string info.name
             ,pair "amount" <| JE.int amount
             ,pair "message" <| JE.string info.message] <|
             JD.succeed NoMsg


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





--updateChatPageInfo : UpdatePage ChatPageMsg ChatPageInfo
updateChatPageInfo model msg chatPageInfo =
  let setChatPageInfo info = {model | page = ChatPage info}
  in case msg of
       ChatPageChatBoxMsg chatBoxMsg -> updateChatBox
         chatPageInfo.chatBox (\newChatBox -> setChatPageInfo <| {chatPageInfo | chatBox = newChatBox})
         model chatBoxMsg (Msg << ChatPageMsg << ChatPageChatBoxMsg)



--updateChatStreamPageInfo : UpdatePage ChatStreamPageMsg ChatStreamPageInfo
updateChatStreamPageInfo model msg chatStreamPageInfo =
  let setChatStreamPageInfo info = {model | page = ChatStreamPage info}
  in case msg of
       ChatStreamPageMessageBoxMsg messageBoxMsg -> updateMessageBox
         chatStreamPageInfo.messageBox (\newMessageBox -> setChatStreamPageInfo {chatStreamPageInfo | messageBox = newMessageBox})
         model messageBoxMsg (Msg << ChatStreamPageMsg << ChatStreamPageMessageBoxMsg)
      -- ChatStreamPageMessageBoxMsg mRoomMsg -> updateMessageBox model mRoomMsg chatStreamPageInfo.messageBox
      --   (ChatStreamPageMsg << ElmBarMsg << ChatStreamPageMessageBoxMsg) <|
      --   \newMRoom -> setChatStreamPageInfo <| {chatStreamPageInfo | messageBox = newMRoom}
--





--updateStreamerPageInfo : UpdatePage StreamerPageMsg StreamerPageInfo
updateStreamerPageInfo model msg streamerPageInfo =
  let setStreamerPageInfo : StreamerPageInfo -> Model
      setStreamerPageInfo info = {model | page = case model.page of
        StreamerPage _ -> StreamerPage <| info
        anyOtherPage -> anyOtherPage}
  in case msg of
       StreamerPageChatBoxMsg chatBoxMsg -> updateChatBox
         streamerPageInfo.chatBox (\newChatBox -> setStreamerPageInfo <| {streamerPageInfo | chatBox = newChatBox})
         model chatBoxMsg (Msg << StreamerPageMsg << StreamerPageChatBoxMsg)
       StreamerPageModRoomMsg chatRoomMsg -> updateChatRoom
         streamerPageInfo.modRoom (\newChatRoom -> setStreamerPageInfo {streamerPageInfo | modRoom = newChatRoom})
         model chatRoomMsg (Msg << StreamerPageMsg << StreamerPageModRoomMsg)
       StreamerPageMentionMessageBoxMsg messageBoxMsg -> updateMessageBox
         streamerPageInfo.atMessageBox (\newMessageBox -> setStreamerPageInfo {streamerPageInfo | atMessageBox = newMessageBox})
         model messageBoxMsg (Msg << StreamerPageMsg << StreamerPageMentionMessageBoxMsg)

       OverStreamerPage f -> noCmd <| setStreamerPageInfo <| f streamerPageInfo
       StreamStatusSetter status -> noCmd <| setStreamerPageInfo {streamerPageInfo | streamStatus = status}
       HostCheck strCheck -> pair model <|
         apiPOSTDefault "mod" "host_check"
           [pair "name" <| JE.string strCheck] <|
           flip JD.map (JD.field "check_result" JD.int) <| \int ->
             Msg <| StreamerPageMsg <| OverStreamerPage <| \info ->
                  if info.hostingSearch /= strCheck then info
                     else  {info | hostingCheck = int}
