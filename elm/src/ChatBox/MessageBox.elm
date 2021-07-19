module ChatBox.MessageBox exposing (..)

import Array
import Color
import Debug
import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlA
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Time exposing (Zone)

--import Chroma.Types as Chr
--import Chroma.Converter.Out.ToRgba as Chr
--import List.Nonempty as NE
import String.Extra exposing (clean)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La
import Element.Keyed as Ky

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)






messageBox : Model -> Bool -> String -> Chat -> MessageBox -> Element MessageBoxMsg
messageBox model isBgAlpha containerName chat mBox =
  co [width fill, height fill, scrollbars]
     -- highlighted users box
     [if mBox.highlightList == []
         then em [height <| px 0
                 --,htmlStyle "transition" "height 0.75s ease-in-out"
                 ]
         else co [width fill, height shrink, paddingRight 16] <|
                 highlightBox model (containerName ++ "messagebox-") chat mBox
     -- main message list
     ,elmUIVBar model mBox.elmBar (Msg << MessageBoxElmBarMsg) (containerName ++ "messagebox-")
        (\viewport elmBarMsg -> BatchMsgs <|
          [Msg <| MessageBoxElmBarMsg elmBarMsg]
          --++
        ) <|
        --La.lazy5
        messageRoomHelper model.commonInfo (containerName ++ "messagebox-") isBgAlpha
                          chat mBox]




highlightBox : Model -> String -> Chat -> MessageBox -> List (Element MessageBoxMsg)
highlightBox model containerName chat mBox =
  let colorPalette = currentColorPalette model
      highlightList = mBox.highlightList
      arrowButton off evt = faIcon
        ([padding 6
         ,Ft.size 24
         ,Bdr.rounded 6]
        ++ colorTransitionStyle
        ++ if off
              then [Ft.color colorPalette.txSoft3]
              else [Ft.color colorPalette.txSoft
                   ,mouseOver
                     [Ft.color colorPalette.bgMain
                     ,Bg.color colorPalette.mainHighlight]
                   ,pointer, Evt.onClick evt]
        )
      setUserBarSelected = Msg << SetHighlightedUserSelected containerName
      userBar = ro
        [width fill, paddingBottom 8] --hBox.users-- name row
        [arrowButton (mBox.userBarSelected == 1)
                     (setUserBarSelected (mBox.userBarSelected - 1))
                     "fas fa-chevron-left"
        ,el [width fill, paddingXY 4 0, clipX] <|
         el [width fill, clipX, htmlID <| containerName ++ "userbar-container"] <|
            ro [spacing 4, Ft.size 16, clipX
               ,htmlID <| containerName ++ "userbar-wrapper"
               ,htmlStyle "margin-left" <| String.fromFloat (negate mBox.userBarMargin) ++ "px"
               ,htmlStyle "transition" "margin 0.75s ease-in-out"] <|
               flip indexedMap highlightList <| \int user ->
                 el [htmlID <| containerName ++ "userbar-" ++ String.fromInt int] <|
                    bubbleActiveMainFA model (int == mBox.userBarSelected) <|
                      ex [paddingXY 8 4, Evt.onClick <| setUserBarSelected int] <|
                         user.username
        ,arrowButton (mBox.userBarSelected == List.length highlightList)
                     (setUserBarSelected <| mBox.userBarSelected + 1)
                     "fas fa-chevron-right"
        ,el [paddingLeft 16] <|
            faIcon ([padding 6
                    ,Ft.size 24
                    ,Bdr.rounded 6
                    ,pointer
                    ,Evt.onClick <| Msg CloseHighlightBox]
                   ++ colorTransitionStyle)
                   "fas fa-times"
        ]
      maybeUserInfo = flip Maybe.andThen (ListE.getAt (mBox.userBarSelected - 1) highlightList) <|
             \user -> flip Maybe.map user.userInfo <| \userInfo -> (user.username, userInfo)
      userInformation = case maybeUserInfo of
        Nothing -> em []
        Just (username, userInfo) -> co
          [paddingLeft 12, spacing 8, Ft.bold, Ft.size 16, Ft.color colorPalette.txSoft] <|
          let usernamePronouns = ro
                [spacing 8]
                [el [Ft.size 20] <|
                    mkChromaUsername model.commonInfo.settings.themeMode
                                     username userInfo.nameColor
                ,elMaybe userInfo.pronouns <| \pronouns ->
                    ex [alignBottom] pronouns]
              accountCreation = elMaybe model.commonInfo.localInfo.zone <| \zone ->
                ex [] <| "Account created on " ++ mkDate zone userInfo.accountCreation
              roleInfo = ro
                [spacing 8]
                [ex [] <|
                    "Joined In Season " ++ String.fromInt userInfo.season
                ,case userInfo.role of
                  Nothing -> Element.none
                  Just role -> ro [] <|
                    --"Season " ++ String.fromInt userInfo.season ++ ":" <|
                    [el [height fill, paddingRight 8] <|
                        em [width <| px 4, height fill, Bdr.rounded 4
                           ,Bg.color colorPalette.bgMain3]
                    ,mkRoleBadge model.commonInfo.staticInfo 16 role
                    ,ex [] <|
                        case role of
                          Subscriber sub -> "Tier " ++ String.fromInt sub.tier
                          Special name -> name ++
                            case userInfo.power of
                              0 -> " (VIP)"
                              1 -> " (Moderator)"
                              _ -> " (Admin)"]
                ]
          in
          -- username, pronouns
          [usernamePronouns
          ,co [paddingLeft 12, spacing 8] <|
              -- account creation
              [accountCreation
              -- role badge, role info
              ,roleInfo]
          ,elMaybe userInfo.modInfo <| \modInfo -> co
            [width fill, spacing 8] <|
            let modAction str evt = el
                  [Evt.onClick evt] <|
                  bubbleMain model str
                modActions = elIf (userInfo.power >= 1) <|
                  ro [spacing 16]
                     [modAction "Ban" <| Msg <| POSTBanUser username
                     ,modAction "1m"  <| Msg <| POSTCensorUser username 60
                     ,modAction "10m" <| Msg <| POSTCensorUser username (10 * 60)
                     ,modAction "30m" <| Msg <| POSTCensorUser username (30 * 60)
                     ,modAction "1h"  <| Msg <| POSTCensorUser username (60 * 60)
                     ,modAction "1d"  <| Msg <| POSTCensorUser username (24 * 60 * 60)
                     ]
                tab str int tabNum =
                  let isTabOn = Maybe.map .open mBox.tray == Just tabNum
                  in el [Evt.onClick <| if isTabOn
                          then Msg <| CloseTray
                          else Msg <| OpenTray tabNum
                        ] <|
                        bubbleActiveMainFA model
                          isTabOn <|
                          co [padding 8, spacing 4]
                             [ex [Ft.size 16, Ft.family [Ft.monospace]] <|
                                 String.fromInt int
                             ,ex [Ft.size 16]
                                 str]
                tabs = ro [paddingBottom 8] <|
                  indexedMap (\int f -> f int)
                    [tab "Messages" modInfo.meaningfulMessages]
            in [modActions
               ,tabs
               ,elMaybe mBox.tray <| \tray ->
                 let trayElmBar topMessageRequest bottomMessageRequest messages =
                       el [width fill, height <| px 200, scrollbars] <|
                       el [width fill, alignBottom, scrollbars] <|
                          elmUIVBar
                            model tray.elmBar (Msg << MessageBoxElmBarMsg)
                              (containerName ++ "highlightbox")
                              (\viewport elmBarMsg -> BatchMsgs
                                [Msg <| HighlightBoxElmBarMsg elmBarMsg
                                ,Msg <| OnScrollHighlightedMessages tray viewport
                                          topMessageRequest bottomMessageRequest])
                                        --OverMessageBox <| \messageBox ->
                                        -- let newTray = messageBox.tray
                                        -- in {messageBox | tray = {newTray | focus = Just <|
                                        --      let addZone = if infiniteScroll.direction
                                        --            then viewport.viewport.y else viewportMarginDown viewport
                                        --      in infiniteScroll.stack +
                                        --           if 100 >= addZone then 1 else 0
                                        --      }
                                        --    }
                              messages
                  in case tray.open of
                       -- list all highlighted users messages
                       1 -> let allMessagesByUser = flip List.map highlightList <| \user ->
                                  mapMaybeToList user.userInfo <| \userInfo_ ->
                                  mapMaybeToList userInfo_.modInfo <| \modInfo_ ->
                                  mapMaybeToList modInfo_.messagesInfo <| \messagesInfo_ ->
                                    let chatUser = ChatUser
                                          user.username
                                          userInfo_.role
                                          userInfo_.badges
                                          userInfo_.pronouns
                                          userInfo_.nameColor
                                    in flip List.map messagesInfo_.messages <| \message ->
                                         pair message.timestamp <| UserMessage <|
                                           UserMessageRecord chatUser message.timestamp message.message
                                allMessagesByDate = List.sortBy Tuple.first <| List.concat allMessagesByUser
                                (leftSlice, rightSlice) = case flip Maybe.andThen tray.focus (\focus -> ListE.getAt (focus - 1) <| List.map Tuple.first allMessagesByDate) of
                                  Nothing -> (0, min 49 <| List.length allMessagesByDate)
                                  Just int_ -> (int_ - 1, min (48 + int_) <| List.length allMessagesByDate)
                                highlightedMessageSlice = Array.toList <|
                                  Array.slice leftSlice rightSlice <| Array.fromList <|
                                  List.map Tuple.second allMessagesByDate
                                --allMessages = highlightMessageRoomHelper
                                --  commonInfo (containerName ++ "highlightbox") False chat.users <|
                                --  addDateMessages <|
                            in trayElmBar
                                 (BatchMsgs
                                   [LogMessage "Top Scroll"
                                   ,NoMsg]) -- Top
                                 (BatchMsgs
                                   [LogMessage "Bottom Scroll"
                                   ,NoMsg]) <| -- Bottom
                                 highlightMessageRoomHelper model.commonInfo (containerName ++ "highlightbox-")
                                  highlightList chat.users highlightedMessageSlice
                       -- list mod actions
                       _ -> Element.none -- trayElmBar () []
               ]
          ]
  in [co [width fill, height fill
         ,Bg.color colorPalette.bgMain]
         [userBar
         ,userInformation
         ]
     ,horizontalLine model
     ]




--------------------------------------------------------------------------------




messageRoomHelper : CommonInfo -> String -> Bool
                 -> Chat -> MessageBox -> Element MessageBoxMsg
messageRoomHelper commonInfo containerName isBgAlpha chat mBox =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
  in Ky.column ([width fill, height fill
                ,chatFontFamily
                ,Ft.color colorPalette.txMain, Ft.size commonInfo.settings.textSize
                ,Bg.color <| if isBgAlpha
                  then rgba255 0 0 0 0 else colorPalette.bgMain
                ]
               ++
               listIf (not <| mBox.highlightList == [] || mBox.hoverUsername)
                 [Evt.onClick <| Msg <| CloseHighlightBox]
               ) <|
               List.map (lazyChatMessage commonInfo containerName isBgAlpha highlightUsernameEvents
                                         chat.users mBox.highlightList)
                        chat.messages

highlightMessageRoomHelper : CommonInfo -> String
                          -> HighlightList -> ChatUserList -> List ChatMessage -> Element MessageBoxMsg
highlightMessageRoomHelper commonInfo containerName highlightList users messages =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
  in Ky.column ([width fill, height fill
                ,chatFontFamily
                ,Ft.color colorPalette.txMain, Ft.size commonInfo.settings.textSize
                ,Bg.color colorPalette.bgMain
                ]
               ) <|
               List.map (lazyChatMessage commonInfo containerName False
                                         (selectHighlightedUsernameEvents highlightList)
                                         users [])
                        messages



lazyChatMessage : CommonInfo -> String -> Bool -> UsernameEvents
               -> ChatUserList -> HighlightList -> ChatMessage -> (String, Element MessageBoxMsg)
lazyChatMessage commonInfo containerName isBgAlpha usernameEvents users hBox message =
  let basicAlpha = hBox == []
      userMessageAlpha = basicAlpha || case message of
        UserMessage userMessage -> List.member userMessage.user.username <| List.map .username hBox
        _ -> False
      chatMessageWrapper alphaRule = pg
        [width fill, height shrink, paddingXY 12 8, spacingXY 0 8
        ,alpha <| if alphaRule then 1 else 0.3
        ]
  in case message of
    RawMessage rawMessage -> pair (String.fromInt rawMessage.timestamp) <|
      chatMessageWrapper basicAlpha <|
      --La.lazy5
      rawMessageFormatter commonInfo containerName isBgAlpha usernameEvents users rawMessage
    SystemMessage systemMessage -> pair (String.fromInt systemMessage.timestamp) <|
      chatMessageWrapper basicAlpha <|
      --La.lazy5
      systemMessageFormatter commonInfo containerName isBgAlpha usernameEvents users systemMessage
    UserMessage usermessage -> pair (String.fromInt usermessage.timestamp) <|
      chatMessageWrapper userMessageAlpha <|
      --La.lazy5
      userMessageFormatter commonInfo containerName isBgAlpha usernameEvents users usermessage
    DelayedUserMessage usermessage -> pair (String.fromInt usermessage.timestamp) <|
      chatMessageWrapper userMessageAlpha <|
      --La.lazy5
      userMessageFormatter commonInfo containerName isBgAlpha usernameEvents users usermessage
    TempUserMessage usermessage -> pair (String.fromInt usermessage.timestamp) <|
      elMaybe commonInfo.profile <| \profile ->
        chatMessageWrapper userMessageAlpha <|
        --La.lazy5
        userMessageFormatter commonInfo containerName isBgAlpha usernameEvents users <|
          UserMessageRecord
            (profileToChatUser profile)
            usermessage.timestamp
            usermessage.message



rawMessageFormatter : CommonInfo -> String -> Bool -> UsernameEvents
                   -> ChatUserList -> RawMessageRecord -> List (Element MessageBoxMsg)
rawMessageFormatter commonInfo containerName isBgAlpha usernameEvents users rawMessage =
  [tx rawMessage.message.unparsed]

systemMessageFormatter : CommonInfo -> String -> Bool -> UsernameEvents
                      -> ChatUserList -> SystemMessageRecord -> List (Element MessageBoxMsg)
systemMessageFormatter commonInfo containerName isBgAlpha usernameEvents users systemMessage =
  [tx systemMessage.message.unparsed]

userMessageFormatter : CommonInfo -> String -> Bool -> UsernameEvents
                    -> ChatUserList -> UserMessageRecord -> List (Element MessageBoxMsg)
userMessageFormatter commonInfo containerName isBgAlpha usernameEvents users userMessage =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      settings = commonInfo.settings
      staticInfo = commonInfo.staticInfo
      paddingResize = paddingRight <| paddingAdjustment settings.textSize
      timeStamp = if settings.showTimestamps
        then ex [paddingResize
                ,Ft.color colorPalette.txMain2] <|
                mkTimeStamp commonInfo.localInfo.zone userMessage.timestamp
        else Element.none
      role = MaybeE.unwrap Element.none (mkRoleBadge staticInfo settings.textSize) userMessage.user.role
      specialBadges = List.map (mkSpecialBadge staticInfo settings.textSize) userMessage.user.badges
      pronouns = flip (MaybeE.unwrap Element.none) userMessage.user.pronouns <|
        ex [Ft.color colorPalette.txMain2]
  in [timeStamp]
     ++ listIf settings.showBadges
               ([role] ++ specialBadges)
     ++
     [pronouns
     ,chromaUsernameBubble commonInfo containerName isBgAlpha usernameEvents
                           userMessage.user.username userMessage.user.nameColor
     ,formatMessage commonInfo containerName isBgAlpha usernameEvents (userMessage.user.role == Nothing)
                    userMessage.message <|
       flip Dict.union users.chatters <| List.foldl (flip Dict.union) Dict.empty <|
         List.concatMap Dict.values users.specialUsers
     ]



--------------------------------------------------------------------------------
-- formatting



formatMessage commonInfo containerName isBgAlpha usernameEvents allowSubOnlyEmotes message users =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      staticInfo = commonInfo.staticInfo
      userList = Dict.keys users
      emoteList = Dict.keys staticInfo.globalEmoteList
               ++ listIf allowSubOnlyEmotes
                    (Dict.keys staticInfo.subOnlyEmoteList)
   in el [spacingXY 0 8, Ft.color colorPalette.txMain] <|
      pg [] <|
         List.map (parsedMessageFormatter commonInfo containerName isBgAlpha usernameEvents) message.parsed


parsedMessageFormatter : CommonInfo -> String -> Bool -> UsernameEvents
                      -> ParserdPiece -> Element MessageBoxMsg
parsedMessageFormatter commonInfo containerName isBgAlpha usernameEvents parsedMessage = case parsedMessage of
  PText str -> tx str
  PUserRef username str -> (flip ex str) <|
    usernameStyle commonInfo isBgAlpha
    ++ usernameEvents containerName username
  PEmote str ->
    let emoteList = Dict.union commonInfo.staticInfo.globalEmoteList commonInfo.staticInfo.subOnlyEmoteList
    in case Dict.get str emoteList of
         Just emote -> emoteWithPadding commonInfo.settings.textSize emote
         _ -> Element.none
  --PLink str ->
  --PItalics msgs -> formatter msgs |> pg
  --  []
  --PBold msgs -> formatter msgs |> pg
  --  []
  --PBoldItal msgs -> formatter msgs |> pg
  --  []
  --PColored msgs -> formatter msgs |> pg
  --  []




--------------------------------------------------------------------------------




chromaUsernameBubble commonInfo containerName isBgAlpha usernameEvents username nameColor =
  el (usernameStyle commonInfo isBgAlpha -- bubble
     ++ usernameEvents containerName username) <|
     mkChromaUsername commonInfo.settings.themeMode username nameColor


usernameStyle : CommonInfo -> Bool -> List (Attribute MessageBoxMsg)
usernameStyle commonInfo isBgAlpha =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      textSize = commonInfo.settings.textSize
  in [Bg.color <| if isBgAlpha then rgba255 0 0 0 0 else colorPalette.bgMain
     ,Bdr.rounded 16
     ,Ft.bold
     ,paddingXY (paddingAdjustment textSize) 0
     ,pointer
     ,mouseOver [Bg.color colorPalette.bgMain2]]
     ++ colorTransitionStyle

type alias UsernameEvents = String -> String -> List (Attribute MessageBoxMsg)

-- for main message list
highlightUsernameEvents containerName username =
  [Evt.onMouseDown <| Msg <| SetHoverUsername True
  ,Evt.onMouseUp <| Msg <| SetHoverUsername False
  ,Evt.onClick <| Msg <| UpdateHighlightUsersList containerName username]

-- for highlighted users message list
selectHighlightedUsernameEvents highlightList containerName username =
  case ListE.findIndex (\u -> u.username == username) highlightList of
    Just int -> [Evt.onClick <| Msg <| SetHighlightedUserSelected containerName int]
    _ -> []



mkRoleBadge staticInfo textSize role = case role of
  Special name -> case Dict.get name staticInfo.specialRoleBadges of
    Just emote -> emoteWithPadding textSize emote
    _ -> Element.none
  Subscriber sub -> case Array.get (sub.tier - 1) staticInfo.subBadges of
      Just badgeList ->
        let subBadge = List.head <| Tuple.first <|
              List.partition ((<=) sub.months << .monthsRequired) badgeList
        in case subBadge of
             Just badge -> emoteWithPadding textSize badge.emote
             _ -> Element.none
      _ -> Element.none

mkSpecialBadge staticInfo textSize badgeName = case Dict.get badgeName staticInfo.specialRoleBadges of
  Just emote -> emoteWithPadding textSize emote
  _ -> Element.none

textSizeAdjustment textSize = textSize - 12
paddingAdjustment textSize = textSizeAdjustment textSize // 2 + 4
emoteWithPadding textSize = el
  [paddingRight <| paddingAdjustment textSize]
  << mkEmote textSize



{-
NOTES

The message box is broken up list this

- HighlightBox
  - highlighted user bar
  - Selected User Info
  - Highlighted Users Message List (recent messages from users in highlight list)
- Main Message List (usernames add names to highlight list)

-}
