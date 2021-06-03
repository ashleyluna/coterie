module Chat.ChatRoom exposing (..)

import Color
import Dict
import Browser.Dom
import Task
import Time exposing (Posix, Zone)

import List.Extra as ListE exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bdr
import Element.Font as Ft
import Element.Input as In
import Element.Events as Evt

import Accessors exposing (..)

import Chat.MessageRoom exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Update.LiveInfo exposing (..)
import Update.CommonInfo exposing (..)
import Update.Chat exposing (..)








chatRoom : Model -> String -> Chat -> ChatRoom -> Element ChatRoomMsg
chatRoom model containerName chat cRoom =
  let colorPalette = currentColorPalette model
  in co [width fill, height fill, scrollbars]
        [el ([width fill, height fill, scrollbars]
             ++
             case cRoom.chatRoomOverlay of
                EmoteOverlay -> List.singleton <| inFront <| emoteOverlay model cRoom
                _ -> []
            ) <| messageRoom model False MessageRoomMsg (containerName ++ "chatroom-") chat cRoom.messageRoom
        -- chat input
        ,ro ([width fill, alignBottom, centerX, paddingXY 12 6, spacing 8
            ,Ft.color colorPalette.txSoft, Ft.size 14
            ,Bg.color colorPalette.bgMain]
            ++ if not cRoom.mentionBox then []
                  else List.singleton <| above <|
                    el [height <| px 120, padding 4, Bg.color <| rgb 0 0 0, clip] <|
                    let mentionChars = String.fromList <| List.reverse <|
                                       ListE.takeWhile Char.isAlphaNum <|
                                       String.toList <| String.reverse cRoom.input
                        filterUsers = Dict.filter <| \name _ ->
                          mentionChars == "" || String.contains (String.toLower mentionChars) (String.toLower name)
                        simpleDict f = Dict.toList << Dict.map (\_ -> f)
                        specialUsers : List (String, NameColor)
                        specialUsers = List.concatMap (\(_,ls) -> ls) <|
                          List.filter (\(_, ls) -> ls /= []) <|
                          List.concatMap (simpleDict <| simpleDict .nameColor << filterUsers) chat.users.specialUsers
                        chatters : List (String, NameColor)
                        chatters = List.take 10 <| simpleDict .nameColor <| filterUsers chat.users.chatters
                    in
                    co [] <|
                       flip List.map (specialUsers ++ chatters) <| \(username,nameColor) ->
                         el [width fill, centerX, padding 8] <|
                            chromaUsername model.commonInfo.settings.themeMode
                                           username nameColor

            )
            [In.multiline [width fill, height (maximum 95 shrink)
                          ,Ft.color colorPalette.txMain
                          ,Bdr.width 2, Bdr.rounded 8, Bdr.color colorPalette.bgMain3
                          ,Bg.color colorPalette.bgMain
                          ,focused [Bdr.color colorPalette.mainHighlight]
                          --,In.focusedOnLoad
                          --,Element.htmlAttribute <| KeyEvt.on KeyEvt.Keyup <| List.singleton <| pair Keyb.Tab <|
                          --  SetMentionBox <| not cRoom.mentionBox
                          ]
                          {onChange = UpdateChatRoomInput
                          ,text = cRoom.input
                          ,placeholder = Just <| In.placeholder [] <| tx "Send a message"
                          ,label = In.labelHidden "Chat Input"
                          ,spellcheck = True}
             -- emote overlay button
            ,el ([centerY, pointer
                 ,Evt.onClick <| SetChatRoomOverlay EmoteOverlay]) <|
             bubbleActiveMainFA model (isChatRoomOverlayOn cRoom EmoteOverlay) <|
             faIcon [padding 6, Ft.size 24]
                    "fas fa-grin-alt"]
        ]



emoteOverlay : Model -> ChatRoom -> Element ChatRoomMsg
emoteOverlay model cRoom =
  let colorPalette = currentColorPalette model
      mkEmoteButton gray emote = el
        [Evt.onClick <| AddEmoteChatRoomInput emote.name
        ,if gray then htmlStyle "filter" "grayscale(100%)" else pointer] <|
        mkEmote 32 emote
  in --mainBoxOverlayWrapper model <|
       co [width fill, height fill, paddingXY 0 16, spacing 40
          ,Ft.size 16, Ft.bold, noSelection]
          [overlayHeaderLeft model "Emotes"
          ,wrappedRow [centerX, spacing 20] <| List.map (mkEmoteButton False) <|
                      Dict.values model.commonInfo.staticInfo.globalEmoteList
          ,ex [centerX, paddingEach {top = 50, right = 0, bottom = 0, left = 0}
              ,Ft.size 20, Ft.color colorPalette.txMain2]
              "Sub-Only Emotes"
          ,wrappedRow [centerX, spacing 20] <| List.map (mkEmoteButton True) <|
                      Dict.values model.commonInfo.staticInfo.subOnlyEmoteList
          ]
