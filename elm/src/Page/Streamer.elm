module Page.Streamer exposing (..)

import Array
import Browser.Dom
import Css
import Css.Animations as CssA
import Css.Transitions as CssT
import Dict
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Lazy as HtmlL
import Maybe.Extra as MaybeE
import Result.Extra as ResultE
import Task
import Time exposing (Posix, Zone)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La

import Chat.ChatBox exposing (..)
import Chat.ChatRoom exposing (..)
import Chat.MessageRoom exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)
import Update.Page exposing (..)
import Update.CommonInfo exposing (..)
import Update.LiveInfo exposing (..)



streamerPage model streamerPageInfo =
  let colorPalette = currentColorPalette model
  in finishedLayout model <| ro
     [width fill, height fill] <|
     [co [width fill, height fill]
         [flip (MaybeE.unwrap Element.none) model.liveInfo.streamStatus <|
           streamStatusSetter model streamerPageInfo
         ,horizontalLine model
         ]
     ,messageBox model streamerPageInfo]


messageBox model streamerPageInfo =
  let colorPalette = currentColorPalette model
      mainChat = model.liveInfo.mainChat
      modChat = model.liveInfo.modChat
      atRoom = {mainChat | messages = List.take 30 <| case model.commonInfo.profile of
        Nothing -> []
        Just profile -> flip List.filter mainChat.messages <|
          \message -> case message of
            UserMessage userMessage -> String.contains
              (String.toLower profile.username)
              (String.toLower userMessage.message.unparsed)
            _ -> False
        }
  in ro [height fill, Bg.color colorPalette.bgMain] <|
        [verticalLine model
        ,co [width <| px 500, height fill] <|
            [Element.map (StreamerPageMsg << StreamerPageModRoomMsg) <|
             chatRoom model ModChat "streamerpage-modchat-" modChat streamerPageInfo.modRoom
            ,horizontalLine model
            ,messageRoom model True (StreamerPageMsg << StreamerPageMentionMessageRoomMsg)
                        "streamerpage-mention-" atRoom streamerPageInfo.atMessageRoom]
        ,verticalLine model
        ,el [width <| px 500, height fill] <|
            Element.map (StreamerPageMsg << StreamerPageChatBoxMsg) <|
            chatBox model MainChat "streamerpage-" mainChat streamerPageInfo.chatBox]




streamStatusSetter : Model -> StreamerPageInfo -> StreamStatus -> Element Msg
streamStatusSetter model streamerPageInfo streamStatus =
  let colorPalette = currentColorPalette model
  in co [width fill,spacing 12, padding 32, Ft.size 16, Ft.color colorPalette.txSoft]
        [ex [Ft.bold, Ft.size 20] <| "Currently " ++
            case streamStatus of
              Streaming _ -> "Streaming"
              Hosting _ -> "Hosting"
              Offline -> "Offline"
        ,case streamStatus of
          Streaming info -> pt [paddingLeft 16] info.title
          Hosting info -> pt [paddingLeft 16] info.title
          Offline -> Element.none
        ,co [width fill, spacing 20]
            [co [width fill, spacing 20]
                [ro [width fill]
                    [el [width <| fillPortion 1] <|
                     el [centerX, Evt.onClick <| StreamerPageMsg <| StreamStatusSetter <| Just True] <|
                        bubbleActiveMain model (Just True == streamerPageInfo.streamStatus)
                          "Streaming"
                    ,el [width <| fillPortion 1] <|
                     el [centerX, Evt.onClick <| StreamerPageMsg <| StreamStatusSetter <| Just False] <|
                        bubbleActiveMain model (Just False == streamerPageInfo.streamStatus)
                          "Hosting"
                    ,el [width <| fillPortion 1] <|
                     el [centerX, Evt.onClick <| StreamerPageMsg <| StreamStatusSetter Nothing] <|
                        bubbleActiveMain model (Nothing == streamerPageInfo.streamStatus)
                          "Offline"
                    ]
                ,el [width fill, centerX] <| case streamerPageInfo.streamStatus of
                    Just True -> In.text (fieldStyle model)
                      {onChange = \str -> StreamerPageMsg <| OverStreamerPage <| \info ->
                        {info | streamingTitle = str}
                      ,text = streamerPageInfo.streamingTitle
                      ,placeholder = placeholder (Ft.color colorPalette.txSoft2) "Title"
                      ,label = In.labelHidden "Streaming Title"}
                    Just False -> fieldCheckMain model In.text
                      streamerPageInfo.hostingCheck
                      (\str -> StreamerPageMsg <| OverStreamerPage <| \info -> {info | hostingSearch = str})
                      (\str -> StreamerPageMsg <| HostCheck str)
                      streamerPageInfo.hostingSearch
                      "Creator's Name"
                      "Hoting Creator Search Input"
                    _ -> em [height <| px 44]

                ]
            ,el [centerX, Evt.onClick <| ChangeStreamStatus <| case streamerPageInfo.streamStatus of
                  Just True -> Just <| Ok streamerPageInfo.streamingTitle
                  Just False -> Just <| Err streamerPageInfo.hostingSearch
                  _ -> Nothing
                ] <|
                bubbleMain model "Change Status"
            ]
        ]
