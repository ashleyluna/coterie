module Chat.MessageRoom exposing (..)

import Array
import Color
import Debug
import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlA
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






messageRoom : Model -> Bool -> (ElmBarMsg MessageRoomMsg -> msg) -> String -> Chat
           -> ElmBar MessageRoom -> Element msg
messageRoom model roomMode liftElmBarMsg containerName chat elmBar =
  let viewport = elmBar.viewport
  in elmUIVBar model liftElmBarMsg (containerName ++ "messageroom-") elmBar
     <| La.lazy4 messageRoomHelper model.commonInfo roomMode chat elmBar.content

messageRoomHelper : CommonInfo -> Bool -> Chat -> MessageRoom -> Element MessageRoomMsg
messageRoomHelper commonInfo roomMode chat mRoom =
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
               ++ if mRoom.highlightedUsers == [] || mRoom.hoverUsername then []
                     else [Evt.onClick <| SetHighlightUsersList []]) <|
               List.map (lazyChatMessage commonInfo roomMode chat.users mRoom.highlightedUsers)
                        chat.messages

lazyChatMessage : CommonInfo -> Bool -> ChatUserList -> HighlightedUsers -> ChatMessage -> (String, Element MessageRoomMsg)
lazyChatMessage commonInfo roomMode users highlightedUsers message = case message of
  RawMessage rawMessage -> pair (String.fromInt rawMessage.time) <|
     rawMessageFormatter commonInfo roomMode users highlightedUsers rawMessage
  SystemMessage systemMessage -> pair (String.fromInt systemMessage.time) <|
     systemMessageFormatter commonInfo roomMode users highlightedUsers systemMessage
  UserMessage usermessage -> pair (String.fromInt usermessage.time) <|
     userMessageFormatter commonInfo roomMode users highlightedUsers usermessage



rawMessageFormatter : CommonInfo -> Bool -> ChatUserList -> HighlightedUsers -> RawMessageRecord -> Element MessageRoomMsg
rawMessageFormatter commonInfo roomMode users highlightedUsers rawMessage =
  chatMessageWrapper commonInfo highlightedUsers False
  [tx rawMessage.message.unparsed]

systemMessageFormatter : CommonInfo -> Bool -> ChatUserList -> HighlightedUsers -> SystemMessageRecord -> Element MessageRoomMsg
systemMessageFormatter commonInfo roomMode users highlightedUsers systemMessage =
  chatMessageWrapper commonInfo highlightedUsers False
  [tx systemMessage.message.unparsed]

userMessageFormatter : CommonInfo -> Bool -> ChatUserList -> HighlightedUsers -> UserMessageRecord -> Element MessageRoomMsg
userMessageFormatter commonInfo roomMode users highlightedUsers userMessage =
  chatMessageWrapper commonInfo highlightedUsers (List.member userMessage.user.username highlightedUsers) <|
  let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
      settings = commonInfo.settings
      staticInfo = commonInfo.staticInfo
      paddingResize = paddingRight <| paddingAdjustment settings.textSize
      timeStamp = if settings.showTimestamps
        then ex [paddingResize
                ,Ft.color colorPalette.txMain2] <|
                mkTimeStamp commonInfo.localInfo.zone userMessage.time
        else Element.none
      role = case userMessage.user.role of
         Just (Special str) -> case Dict.get str staticInfo.specialRoleBadges of
           Just emote -> emoteWithPadding settings.textSize emote
           _ -> Element.none
         Just (Subscriber sub) -> case Array.get (sub.tier - 1) staticInfo.subBadges of
           Just badgeList ->
             let subBadge = List.head <| Tuple.first <|
                   List.partition ((<=) sub.months << .monthsRequired) badgeList
             in case subBadge of
                  Just badge -> emoteWithPadding settings.textSize badge.emote
                  _ -> Element.none
           _ -> Element.none
         _ -> Element.none
      specialBadges = flip List.map userMessage.user.badges <| \badgeName ->
        case Dict.get badgeName staticInfo.specialRoleBadges of
             Just emote -> emoteWithPadding settings.textSize emote
             _ -> Element.none
      pronouns = flip (MaybeE.unwrap Element.none) userMessage.user.pronouns <|
        ex [Ft.color colorPalette.txMain2]
  in [timeStamp]
     ++ listIf settings.showBadges
               ([role] ++ specialBadges)
     ++
     [pronouns
     ,chromaUsernameBubble commonInfo roomMode userMessage.user.username userMessage.user.nameColor
     ,formatMessage commonInfo roomMode userMessage.user.role userMessage.message <|
       flip Dict.union users.chatters <| List.foldl (flip Dict.union) Dict.empty <|
         List.concatMap Dict.values users.specialUsers
     ]



--------------------------------------------------------------------------------

chatMessageWrapper : CommonInfo -> HighlightedUsers -> Bool -> List (Element MessageRoomMsg) -> Element MessageRoomMsg
chatMessageWrapper commonInfo highlightedUsers otherAlphaRule = pg
  [width fill, height shrink, paddingXY 12 8, spacingXY 0 8
  ,alpha <| if highlightedUsers == [] || otherAlphaRule then 1 else 0.3
  ]

chromaUsernameBubble commonInfo roomMode username nameColor =
  let paddingResize = paddingRight <| paddingAdjustment commonInfo.settings.textSize
  in el (usernameStyle commonInfo roomMode username -- bubble
        ++ colorTransitionStyle) <|
     chromaUsername commonInfo.settings.themeMode username nameColor


usernameStyle : CommonInfo -> Bool -> String -> List (Attribute MessageRoomMsg)
usernameStyle commonInfo roomMode name =
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
     ,Evt.onClick <| UpdateHighlightUsersList name]


formatMessage commonInfo roomMode role message users =
    let colorPalette = currentColorPalette_ commonInfo.settings.themeMode
        staticInfo = commonInfo.staticInfo
        userList = Dict.keys users
        emoteList = Dict.keys staticInfo.globalEmoteList
                 ++ if role == Nothing then []
                       else Dict.keys staticInfo.subOnlyEmoteList
     in el [spacingXY 0 8, Ft.color colorPalette.txMain] <|
        pg [] <|
           List.map (parsedMessageFormatter commonInfo roomMode) message.parsed


parsedMessageFormatter : CommonInfo -> Bool -> ParserdPiece -> Element MessageRoomMsg
parsedMessageFormatter commonInfo roomMode parsedMessage = case parsedMessage of
  PText str -> tx str
  PUserRef username str -> (flip ex str) <|
    []
    ++ usernameStyle commonInfo roomMode username
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
