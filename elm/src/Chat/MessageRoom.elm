module Chat.MessageRoom exposing (..)

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






messageRoom : Model -> Bool -> (MessageRoomMsg -> msg) -> String -> Chat
           -> MessageRoom -> Element msg
messageRoom model roomMode liftMessageRoomMsg containerName chat mRoom =
  let modUserInfoBoxOn = mRoom.highlightBox /= [] && profileIsMod model.commonInfo.profile
  in Element.map liftMessageRoomMsg <|
     co [width fill, height fill, scrollbars]
        [co [width fill, height <| if modUserInfoBoxOn then shrink else px 0
            ,paddingRight 16
            ] <| listIf modUserInfoBoxOn <|
                   highlightBox model (containerName ++ "messageroom-") mRoom
        ,elmUIVBar model MessageRoomElmBarMsg
           (containerName ++ "messageroom-") mRoom.elmBar <|
           La.lazy5 messageRoomHelper model.commonInfo (containerName ++ "messageroom-") roomMode chat mRoom]

highlightBox : Model -> String -> MessageRoom -> List (Element MessageRoomMsg)
highlightBox model containerName mRoom =
  let colorPalette = currentColorPalette model
      hBox = mRoom.highlightBox
      button off evt = faIcon
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
      setSelectedUser = SetHighlightedUserFocus containerName
  in [co [width fill, height fill
         ,Bg.color colorPalette.bgMain
         ]
         -- user bar
         [ro [width fill, paddingBottom 8] --hBox.users-- name row
             [button (mRoom.selectedUser == 0)
                     (setSelectedUser (mRoom.selectedUser - 1))
                     "fas fa-chevron-left"
             ,el [width fill, paddingXY 4 0, clipX] <|
              el [width fill, clipX
                 ,htmlID <| containerName ++ "userbar-container"] <|
                 ro [spacing 4, Ft.size 16, clipX
                    ,htmlID <| containerName ++ "userbar-wrapper"
                    ,htmlStyle "margin-left" <| String.fromFloat (negate mRoom.userBarMargin) ++ "px"
                    ,htmlStyle "transition" "margin 0.75s ease-in-out"
                    ,htmlStyle "-webkit-transition" "margin 0.75s ease-in-out"] <|
                    flip List.indexedMap hBox <| \int user ->
                      el [htmlID <| containerName ++ String.fromInt int] <|
                         bubbleActiveMainFA model (int == mRoom.selectedUser) <|
                           ex [paddingXY 8 4
                              ,Evt.onClick <| setSelectedUser int
                              ] <|
                              user.username
             ,button (mRoom.selectedUser == List.length hBox - 1)
                     (setSelectedUser <| mRoom.selectedUser + 1)
                     "fas fa-chevron-right"
             ,el [paddingLeft 16] <|
                 faIcon ([padding 6
                         ,Ft.size 24
                         ,Bdr.rounded 6
                         ,pointer
                         ,Evt.onClick NoHighlights]
                        ++ colorTransitionStyle)
                        "fas fa-times"
             ]
         -- user info
         ,let maybeUserInfo = flip Maybe.andThen (ListE.getAt mRoom.selectedUser hBox) <|
                \user -> flip Maybe.map user.userInfo <| \userInfo -> (user.username, userInfo)
          in case maybeUserInfo of
            Nothing -> em []
            Just (username, userInfo) -> co
              [paddingLeft 12, spacing 8, Ft.bold, Ft.size 16, Ft.color colorPalette.txSoft]
              -- username, pronouns
              [ro [spacing 8]
                  [el [Ft.size 20] <|
                      mkChromaUsername model.commonInfo.settings.themeMode
                                       username userInfo.nameColor
                  ,ex [alignBottom] <|
                      Maybe.withDefault "" userInfo.pronouns]
              ,co [paddingLeft 12, spacing 8] <|
                  -- account creation
                  [elMaybe model.commonInfo.localInfo.zone <| \zone -> ex [] <|
                    "Account created on " ++ mkDate zone userInfo.accountCreation
                  -- role badge, role info
                  ,ro [spacing 8]
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
                   -- users badges, role, etc
                   -- mod actions
                   -- messages
                  ]
              ]
         ]
     ,horizontalLine model
     ]

messageRoomHelper : CommonInfo -> String -> Bool -> Chat -> MessageRoom -> Element MessageRoomMsg
messageRoomHelper commonInfo containerName roomMode chat mRoom =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
  in Ky.column ([width fill, height fill
                ,Ft.family [Ft.typeface "Roboto"
                           ,Ft.typeface "Lato"
                           ,Ft.typeface "Open Sans"
                           ,Ft.typeface "Helvetica"
                           ,Ft.typeface "Verdana"
                           ,Ft.sansSerif
                           ]
                ,Ft.color colorPalette.txMain, Ft.size commonInfo.settings.textSize
                ,Bg.color <| if roomMode
                  then rgba255 0 0 0 0 else colorPalette.bgMain
                ]
               ++
               listIf (not <| mRoom.highlightBox == [] || mRoom.hoverUsername)
                 [Evt.onClick NoHighlights]
               ) <|
               List.map (lazyChatMessage commonInfo containerName roomMode chat.users mRoom.highlightBox)
                        chat.messages

lazyChatMessage : CommonInfo -> String -> Bool -> ChatUserList -> HighlightBox -> ChatMessage -> (String, Element MessageRoomMsg)
lazyChatMessage commonInfo containerName roomMode users hBox message = case message of
  RawMessage rawMessage -> pair (String.fromInt rawMessage.time) <|
     rawMessageFormatter commonInfo containerName roomMode users hBox rawMessage
  SystemMessage systemMessage -> pair (String.fromInt systemMessage.time) <|
     systemMessageFormatter commonInfo containerName roomMode users hBox systemMessage
  UserMessage usermessage -> pair (String.fromInt usermessage.time) <|
     userMessageFormatter commonInfo containerName roomMode users hBox usermessage
  DelayedUserMessage usermessage -> pair (String.fromInt usermessage.time) <|
     userMessageFormatter commonInfo containerName roomMode users hBox usermessage
  TempUserMessage usermessage -> pair (String.fromInt usermessage.time) <|
     case commonInfo.profile of
       Just profile ->
         {- TODO
         userMessageFormatter commonInfo roomMode users hBox <|
           UserMessageRecord
             (profileToChatUser profile)
             usermessage.time
             usermessage.message
         -}
         co []
            [ex [] "temp: "
            ,userMessageFormatter commonInfo containerName roomMode users hBox <|
              UserMessageRecord
                (profileToChatUser profile)
                usermessage.time
                usermessage.message]
       _ -> Element.none



rawMessageFormatter : CommonInfo -> String -> Bool -> ChatUserList -> HighlightBox -> RawMessageRecord -> Element MessageRoomMsg
rawMessageFormatter commonInfo containerName roomMode users hBox rawMessage =
  chatMessageWrapper commonInfo hBox False
  [tx rawMessage.message.unparsed]

systemMessageFormatter : CommonInfo -> String -> Bool -> ChatUserList -> HighlightBox -> SystemMessageRecord -> Element MessageRoomMsg
systemMessageFormatter commonInfo containerName roomMode users hBox systemMessage =
  chatMessageWrapper commonInfo hBox False
  [tx systemMessage.message.unparsed]

userMessageFormatter : CommonInfo -> String -> Bool -> ChatUserList -> HighlightBox -> UserMessageRecord -> Element MessageRoomMsg
userMessageFormatter commonInfo containerName roomMode users hBox userMessage =
  chatMessageWrapper commonInfo hBox (List.member userMessage.user.username <| List.map .username hBox) <|
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      settings = commonInfo.settings
      staticInfo = commonInfo.staticInfo
      paddingResize = paddingRight <| paddingAdjustment settings.textSize
      timeStamp = if settings.showTimestamps
        then ex [paddingResize
                ,Ft.color colorPalette.txMain2] <|
                mkTimeStamp commonInfo.localInfo.zone userMessage.time
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
     ,chromaUsernameBubble commonInfo containerName roomMode userMessage.user.username userMessage.user.nameColor
     ,formatMessage commonInfo containerName roomMode userMessage.user.role userMessage.message <|
       flip Dict.union users.chatters <| List.foldl (flip Dict.union) Dict.empty <|
         List.concatMap Dict.values users.specialUsers
     ]



--------------------------------------------------------------------------------

chatMessageWrapper : CommonInfo -> HighlightBox -> Bool -> List (Element MessageRoomMsg) -> Element MessageRoomMsg
chatMessageWrapper commonInfo hBox otherAlphaRule = pg
  [width fill, height shrink, paddingXY 12 8, spacingXY 0 8
  ,alpha <| if hBox == [] || otherAlphaRule then 1 else 0.3
  ]

chromaUsernameBubble commonInfo containerName roomMode username nameColor =
  let paddingResize = paddingRight <| paddingAdjustment commonInfo.settings.textSize
  in el (usernameStyle commonInfo containerName roomMode username -- bubble
        ++ colorTransitionStyle) <|
     mkChromaUsername commonInfo.settings.themeMode username nameColor


usernameStyle : CommonInfo -> String -> Bool -> String -> List (Attribute MessageRoomMsg)
usernameStyle commonInfo containerName roomMode name =
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      textSize = commonInfo.settings.textSize
  in [Bg.color <| if roomMode
       then rgba255 0 0 0 0 else colorPalette.bgMain
     ,Bdr.rounded 16
     ,Ft.bold
     ,paddingXY (paddingAdjustment textSize) 0
     ,pointer
     ,mouseOver [Bg.color colorPalette.bgMain2]
     ,Evt.onMouseDown <| SetHoverUsername True
     ,Evt.onMouseUp <| SetHoverUsername False
     ,Evt.onClick <| UpdateHighlightUsersList containerName name]


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

formatMessage commonInfo containerName roomMode role message users =
    let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
        staticInfo = commonInfo.staticInfo
        userList = Dict.keys users
        emoteList = Dict.keys staticInfo.globalEmoteList
                 ++ if role == Nothing then []
                       else Dict.keys staticInfo.subOnlyEmoteList
     in el [spacingXY 0 8, Ft.color colorPalette.txMain] <|
        pg [] <|
           List.map (parsedMessageFormatter commonInfo containerName roomMode) message.parsed


parsedMessageFormatter : CommonInfo -> String -> Bool -> ParserdPiece -> Element MessageRoomMsg
parsedMessageFormatter commonInfo containerName roomMode parsedMessage = case parsedMessage of
  PText str -> tx str
  PUserRef username str -> (flip ex str) <|
    []
    ++ usernameStyle commonInfo containerName roomMode username
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


textSizeAdjustment textSize = textSize - 12
paddingAdjustment textSize = textSizeAdjustment textSize // 2 + 4
emoteWithPadding textSize = el
  [paddingRight <| paddingAdjustment textSize]
  << mkEmote textSize
