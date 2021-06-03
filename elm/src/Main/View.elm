module Main.View exposing (view)

import Array
import Browser exposing (Document)
import Browser.Dom
import Css
import Css.Animations as CssA
import Css.Transitions as CssT
import Debug
import Dict
import Html exposing (Html, iframe)
import Html.Attributes as HtmlA
import Html.Lazy as HtmlL
import Http
import Time exposing (Posix, Zone)
import Json.Decode as D
import Task
import Url

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt
import Element.Lazy as La

import Svg
import Svg.Attributes as SvgA


import Chat.ChatBox exposing (..)
import Chat.ChatRoom exposing (..)
import Chat.MessageRoom exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Page.Home exposing (..)
import Page.Streamer exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)
import Update.Page exposing (..)



view :  Model -> Document Msg
view model =
  {title = streamerInfo.siteTitle
  ,body = List.singleton <| case model.page of
    HomePage homePageInfo -> homePage model homePageInfo
    ChatPage chatPageInfo -> chatPage model chatPageInfo
    ChatStreamPage chatStreamPageInfo -> chatStreamPage model chatStreamPageInfo
    StreamerPage streamerPageInfo -> streamerPage model streamerPageInfo
    _ -> Html.div [] []
  }


--------------------------------------------------------------------------------
-- Pages

chatPage model chatPageInfo =
  let colorPalette = currentColorPalette model
      mainChat = model.liveInfo.mainChat
  in finishedLayout model <| el
     [width fill, height fill] <|
     Element.map (ChatPageMsg << ChatPageChatBoxMsg) <|
       chatBox model "chatpage-" mainChat chatPageInfo.chatBox


chatStreamPage model chatStreamInfo =
  let colorPalette = currentColorPalette model
      commonInfo = model.commonInfo
      mainChat = model.liveInfo.mainChat
      streamCommonInfo = {commonInfo | profile = Nothing
                                     , settings = streamerInfo.streamChatSettings}
  in finishedLayout model <| el
     [width fill, height fill] <|
     --Element.map (ChatStreamPageMsg << ChatStreamPageMessageRoomMsg) <|
       messageRoom model True (ChatStreamPageMsg << ChatStreamPageMessageRoomMsg)
                   "chatstreampage-" mainChat chatStreamInfo.messageRoom
