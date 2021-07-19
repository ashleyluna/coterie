module ChatBox.ChatRoom exposing (..)

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

import Keyboard as Keyb exposing (..)
import Keyboard.Events as KeyEvt exposing (..)

import ChatBox.MessageBox exposing (..)
import Internal.Internal exposing (..)
import Internal.Style exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Streamer exposing (..)
import Update.LiveInfo exposing (..)
import Update.CommonInfo exposing (..)
import Update.Chat exposing (..)








chatRoom : Model -> ChatLocale -> String -> Chat -> ChatRoom -> Element ChatRoomMsg
chatRoom model chatLocale containerName chat cRoom =
  let colorPalette = currentColorPalette model
      mentionChars = lastWord cRoom.input
      filterUsers = Dict.filter <| \name _ ->
        mentionChars == "" || String.contains (String.toLower mentionChars) (String.toLower name)
      simpleDict f = Dict.toList << Dict.map (\_ -> f)
      specialUsers : List (String, NameColor)
      specialUsers = List.concatMap (\(_,ls) -> ls) <|
        List.filter (\(_, ls) -> ls /= []) <|
        List.concatMap (simpleDict <| simpleDict .nameColor << filterUsers) chat.users.specialUsers
      chatters : List (String, NameColor)
      chatters = simpleDict .nameColor <| filterUsers chat.users.chatters
      userList = List.take 4 <| specialUsers ++ chatters
  in co [width fill, height fill, scrollbars]
        [el ([width fill, height fill, scrollbars]
            ++ case cRoom.chatRoomOverlay of
                 EmoteOverlay -> List.singleton <| inFront <| emoteOverlay model cRoom
                 _ -> [] ) <|
            Element.map (Msg << MessageBoxMsg) <|
              messageBox model False (containerName ++ "chatroom-") chat cRoom.messageBox
        -- chat input
        ,ro ([width fill, alignBottom, centerX, paddingXY 12 6, spacing 8
             ,Ft.color colorPalette.txSoft, Ft.size model.commonInfo.settings.textSize
             ,Bg.color colorPalette.bgMain]
            -- mention box
            ++ listIf (cRoom.mentionBox /= Nothing)
               [above <|
                  el [width fill, paddingRight 16, clip] <|
                  co [width fill, alignBottom, padding 4
                     ,Bg.color colorPalette.bgMain] <|
                  if userList == []
                     then [ex [padding 4] "No Users Found"]
                     else flip List.indexedMap userList <| \int (username,nameColor) ->
                       -- TODO mentionBox is infront of auto scroll button
                       el [width fill, centerX, padding 4
                          ,Bg.color <| if Just int == cRoom.mentionBox
                            then colorPalette.bgMain2
                            else colorPalette.bgMain
                          ,Evt.onClick <| Msg <| AddMention username] <|
                          mkChromaUsername model.commonInfo.settings.themeMode
                                           username nameColor])
            [In.multiline [width fill, height (maximum 95 shrink)
                          ,Ft.color colorPalette.txMain
                          ,Bdr.width 2, Bdr.rounded 8, Bdr.color colorPalette.bgMain3
                          ,Bg.color colorPalette.bgMain
                          ,focused [Bdr.color colorPalette.mainHighlight]
                          --,In.focusedOnLoad
                          ,Element.htmlAttribute <| KeyEvt.customPerKey KeyEvt.Keydown <|
                            let arrowEvent arrow direction = pair arrow <| -- direction: True == Up, False == Down
                                  {message = case cRoom.mentionBox of
                                    Nothing -> NoMsg
                                    Just position -> Msg <| SetMentionBox <| Just <|
                                      if direction
                                         then max (position - 1) 0
                                         else min (position + 1) <| max (List.length userList - 1) 0
                                  ,preventDefault = cRoom.mentionBox /= Nothing, stopPropagation = False}
                            in [pair Keyb.Tab <|
                                 {message = case cRoom.mentionBox of
                                   Nothing -> case mentionChars of
                                     "" -> NoMsg
                                     _ -> Msg <| SetMentionBox <| Just 0
                                   Just position -> BatchMsgs
                                     [Msg <| SetMentionBox Nothing
                                     ,case ListE.getAt position userList of
                                       Nothing -> NoMsg
                                       Just (str,_) -> Msg <| AddMention str
                                     ]
                                 ,preventDefault = True, stopPropagation = False}
                               ,pair Keyb.Enter <|
                                 {message = if String.length cRoom.input >= 1 && String.length cRoom.input <= 500
                                   then BatchMsgs
                                     [Msg <| OverChatRoom <| \cRoom_ ->
                                       {cRoom_ | input = "", mentionBox = Nothing}
                                     ,UseNow <| Msg << SendUserMessage chatLocale cRoom.input << Time.posixToMillis
                                     ]
                                   else NoMsg -- TODO tell user to not send messages 500+ characters long
                                 ,preventDefault = True, stopPropagation = False}
                               ,arrowEvent Keyb.ArrowUp True
                               ,arrowEvent Keyb.ArrowDown False
                               ]
                          ]
                          {onChange = \str -> if String.length str <= 500
                            then Msg <| OverChatRoom <| \cRoom_ ->
                                   {cRoom_ | input = str
                                           , mentionBox = if lastWord str == ""
                                               then Nothing else cRoom_.mentionBox}
                            else NoMsg
                          ,text = cRoom.input
                          ,placeholder = Just <| In.placeholder [] <| tx "Send a message"
                          ,label = In.labelHidden "Chat Input"
                          ,spellcheck = True}
             -- emote overlay button
            ,el ([centerY, pointer
                 ,Evt.onClick <| Msg <| SetChatRoomOverlay EmoteOverlay]) <|
             bubbleActiveMainFA model (isChatRoomOverlayOn cRoom EmoteOverlay) <|
             faIcon [padding 6, Ft.size 24]
                    "fas fa-grin-alt"]
        ]



emoteOverlay : Model -> ChatRoom -> Element ChatRoomMsg
emoteOverlay model cRoom =
  let colorPalette = currentColorPalette model
      mkEmoteButton gray emote = el
        [Evt.onClick <| Msg <| AddEmoteChatRoomInput emote.name
        ,if gray then htmlStyle "filter" "grayscale(100%)" else pointer] <|
        mkEmote 24 emote
  in chatRoomOverlayWrapper model "Emotes" <|
          [wr [centerX, paddingTop 40 , spacing 20] <| List.map (mkEmoteButton False) <|
                      Dict.values model.commonInfo.staticInfo.globalEmoteList
          ,ex [centerX, paddingEach {top = 50, right = 0, bottom = 0, left = 0}
              ,Ft.size 20, Ft.color colorPalette.txMain2]
              "Sub-Only Emotes"
          ,wrappedRow [centerX, spacing 20] <| List.map (mkEmoteButton True) <|
                      Dict.values model.commonInfo.staticInfo.subOnlyEmoteList
          ]
