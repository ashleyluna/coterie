module Update.ChatBox exposing (..)

import Char.Extra as CharE exposing (..)
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListE exposing (..)
import Task
import Time exposing (Posix, Zone)

import String.Extra exposing (clean)

import Internal.Internal exposing (..)
import Internal.Json exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)
import Update.ElmBar exposing (..)

--
--
--
--
--  ,chatRoom : List ChatRoomMessage
--  ,highlightUsers : List String
--  --,autoScroll : Bool
--
--  ---- Emotes
--  --,emoteList : List Emote
--  }




--------------------------------------------------------------------------------




updateChatBox : ChatLocale -> UpdateElement ChatBoxMsg ChatBox
updateChatBox chatLocale model msg chatBox liftChatBoxMsg setChatBox =
  let a = 1
  in case msg of
       MsgChatBoxMsg msg_ -> pair model <| cmdMsg msg_
       BatchChatBoxMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatBoxMsg) msgs
       ChatRoomMsg chatRoomMsg -> updateChatRoom chatLocale model chatRoomMsg
         chatBox.chatRoom (liftChatBoxMsg << ChatRoomMsg)
         <| \newChatRoom -> setChatBox {chatBox | chatRoom = newChatRoom}
       WaitHalfASecChatBox message -> pair model <| do <|
         waitHalfASec <| Task.succeed <| liftChatBoxMsg message
       ChatBoxElmBarMsg elmBarMsg -> updateElmBar_ model
         elmBarMsg chatBox.elmBar
         (liftChatBoxMsg << ChatBoxElmBarMsg)
         <| \newElmBar -> setChatBox {chatBox | elmBar = newElmBar}


       SetChatBoxOverlay chatBoxOverlay ->
         (setChatBox {chatBox | chatBoxOverlay =
           if isChatBoxOverlayOn chatBox chatBoxOverlay
              then NoChatBoxOverlay else chatBoxOverlay}
         ,cmdMsg <| liftChatBoxMsg <| BatchChatBoxMsgs
            [ChatRoomMsg <| SetChatRoomOverlay NoChatRoomOverlay
            ,ChatRoomMsg <| SetMentionBox Nothing
            ,ChatBoxElmBarMsg ResetScrollStack])
       UpdateChatBoxOverlay chatBoxOverlay -> noCmd <| setChatBox
         {chatBox | chatBoxOverlay = chatBoxOverlay}
       OverSettingsOverlay f -> noCmd <| setChatBox <| case chatBox.chatBoxOverlay of
         SettingsOverlay info -> {chatBox | chatBoxOverlay = SettingsOverlay <| f info}
         _ -> chatBox
       OverCommunityOverlay f -> noCmd <| setChatBox <| case chatBox.chatBoxOverlay of
         CommunityOverlay info -> {chatBox | chatBoxOverlay = CommunityOverlay <| f info}
         _ -> chatBox
       SlideChatBoxOverlayHeader bool direction overlayInfo -> if bool
         then pair (setChatBox
                      {chatBox | chatBoxOverlay = overlayInfo
                               , initiateChatBoxOverlayHeaderSliding = True
                               , chatBoxSlideDirection = direction})
              <| do <| waitAFrame <| Task.succeed <| liftChatBoxMsg
                    <| SlideChatBoxOverlayHeader False direction overlayInfo
         else noCmd <| setChatBox
                {chatBox | initiateChatBoxOverlayHeaderSliding = False}


       -- Register
       SignUpCheck strCheck -> pair model
         <| case chatBox.chatBoxOverlay of
              RegisterOverlay info -> if info.username == strCheck
                then do <| Task.succeed <| liftChatBoxMsg <|
                           POSTSignUpCheck strCheck
                else Cmd.none
              _ -> Cmd.none
       POSTSignUpCheck strCheck -> pair model <| cmdMsg <|
         ApiPOST [] "register/check" "check_username"
           [pair "username" <| JE.string strCheck]
           (JD.map (liftChatBoxMsg << UpdateSignupCheck strCheck)
                  (JD.field "check_result" JD.int))
           <| defaultRequestResponse
       {- this message could exist in POSTSignUpCheck, but if the response takes
          a long time, it could break the page by resetting the model -}
       UpdateSignupCheck strCheck int -> noCmd <| setChatBox <|
         {chatBox | chatBoxOverlay = case chatBox.chatBoxOverlay of
           RegisterOverlay info -> RegisterOverlay <| if info.username == strCheck
             then {info | invalidUsername = int} else info
           _ -> chatBox.chatBoxOverlay}
       -- TODO Remove
       --POSTSignUp info -> pair model <| cmdMsg <| ApiPOST [] "register/check" "signUp"
       --  [pair "username" <| JE.string info.username
       --  ,pair "email" <| JE.string info.email
       --  ,pair "password" <| JE.string info.password]
       --  <| JD.map3 (\int1 int2 int3 -> BatchMsgs <| List.map HomePageMsg
       --         [UpdateSignupCheck "username" info.username int1
       --         ,UpdateSignupCheck "email" info.email int2
       --         ,UpdateSignupCheck "password" info.password int3])
       --         (JD.field "username" JD.int)
       --         (JD.field "email" JD.int)
       --         (JD.field "password" JD.int)

       StartGoogleSignIn -> pair model <| startGoogleSignIn ()

       -- Settings
       PronounsCheck str -> pair model <|
         case chatBox.chatBoxOverlay of
           SettingsOverlay info -> if info.pronouns == str
             then cmdMsg <| liftChatBoxMsg <| POSTPronounsCheck str
             else Cmd.none
           _ -> Cmd.none
       POSTPronounsCheck str -> pair model <| cmdMsg <|
         ApiPOST [] "profile" "check_pronouns"
           [pair "pronouns" <| JE.string str]
           (flip JD.map (JD.field "check_result" JD.int) <| \int ->
             liftChatBoxMsg <| OverSettingsOverlay <| \info ->
                  if info.pronouns /= str then info
                     else  {info | invalidPronouns = int})
           defaultRequestResponse
       POSTSetPronouns str -> pair model <| cmdMsg <|
         ApiPOST [] "profile" "set_pronouns"
           [pair "pronouns" <| JE.string str]
           (flip JD.map (JD.field "pronouns" <| JD.nullable JD.string) <| \newStr ->
             OverProfile <| \profile ->
               {profile | pronouns = newStr}
           )
           defaultRequestResponse
       POSTEquipBadge str int -> pair model <| cmdMsg <|
         ApiPOST [] "profile" "equip_badge"
           [pair ("equip_" ++ String.fromInt int) <| JE.string str]
           (flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             OverProfile <| setProfileBadges firstBadge secondBadge))
           defaultRequestResponse
       POSTUnEquipBadge int -> pair model <| cmdMsg <|
         ApiPOST [] "profile" ("unequip_badge_" ++ String.fromInt int)
           []
           (flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             OverProfile <| setProfileBadges firstBadge secondBadge))
           defaultRequestResponse
       POSTSetDefaultColor maybeColor -> pair model <| cmdMsg <|
         ApiPOST [] "profile" "set_default_color"
           [pair "default_color" <| jeMaybe jeDefaultNameColor maybeColor]
           (flip JD.map jdDefaultNameColor (\c -> BatchMsgs
             [liftChatBoxMsg <| OverSettingsOverlay <| \info ->
               {info | defaultNameColor = maybeColor}
             ,OverProfile <| \profile ->
               let nameColor = profile.nameColor
               in {profile | nameColor =
                  {nameColor | defaultNameColor = c}}]))
           defaultRequestResponse
       POSTSetNameColor left right mode -> pair model <| cmdMsg <|
         ApiPOST [] "profile" "set_name_color"
           [pair "left" <| jeChromaColor left
           ,pair "right" <| jeChromaColor right
           ,pair "mode" <| JE.string <| showChromaMode mode]
           (JD.map3 (\left_ right_ mode_ -> BatchMsgs
             [OverProfile <| \profile -> {profile | nameColor =
               ProfileNameColorRecord profile.nameColor.defaultNameColor
                 left_ right_ mode_}
             ,liftChatBoxMsg <| OverSettingsOverlay <| \info ->
                {info | mode = mode_}])
             (JD.field "left" <| JD.nullable jdChromaColor)
             (JD.field "right" <| JD.nullable jdChromaColor)
             (JD.field "mode" jdChromaMode))
           <| \response -> case response of
               Err err -> httpErrorMsg err
               Ok resMsg -> resMsg
       --POSTSetMode mode -> pair model <| cmdMsg <|
       --  ApiPOST [] "profile" "set_mode"
       --    [pair "mode" <| JE.string <| showChromaMode mode]
       --    (JD.succeed <| liftChatBoxMsg <| OverSettingsOverlay <| \info ->
       --      {info | mode = mode})
       --    <| \response -> case response of
       --        Err err -> httpErrorMsg err
       --        Ok resMsg -> resMsg
       ChatBoxTriggerAutoScrollDown str -> pair model <| cmdMsg <|
         liftChatBoxMsg <| BatchChatBoxMsgs <|
           [ChatRoomMsg <| MessageRoomMsg <|
             GetElmBarViewport (str ++ "chatbox-chatroom-messageroom-elmbar") (AutoScrollDown False)]
           ++ let autoScroll = ChatBoxElmBarMsg << flip GetElmBarViewport (AutoScrollDown False)
              in case chatBox.chatBoxOverlay of
                   CommunityOverlay _ -> [autoScroll <| str ++ "chatbox-community-overlay"]
                   WhispersOverlay _ -> [autoScroll <| str ++ "chatbox-whispers-overlay"]
                   --SettingsOverlay _ -> [autoScroll <| str ++ "chatbox-settings-overlay"]
                   _ -> []


updateChatRoom : ChatLocale -> UpdateElement ChatRoomMsg ChatRoom
updateChatRoom chatLocale model msg chatRoom liftChatRoomMsg setChatRoom =
  let a = 1
  in case msg of
       MsgChatRoomMsg msg_ -> pair model <| cmdMsg msg_
       BatchChatRoomMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatRoomMsg) msgs
       MessageRoomMsg elmBarMsg -> updateElmBar updateMessageRoom model
         elmBarMsg chatRoom.messageRoom
         (liftChatRoomMsg << MessageRoomMsg)
         <| \newMessageRoom -> setChatRoom {chatRoom | messageRoom = newMessageRoom}
       --MessageRoomMsg messageRoomMsg -> updateMessageRoom model messageRoomMsg
       --  chatRoom.messageRoom (liftChatRoomMsg << MessageRoomMsg)
       --  <| \newMessageRoom -> setChatRoom {chatRoom | messageRoom = newMessageRoom}

       SetChatRoomOverlay chatRoomOverlay ->
         (setChatRoom
           {chatRoom | chatRoomOverlay =
             if isChatRoomOverlayOn chatRoom chatRoomOverlay
                then NoChatRoomOverlay else chatRoomOverlay}
           ,cmdMsg <| liftChatRoomMsg <| SetMentionBox Nothing)
       UpdateChatRoomOverlay chatRoomOverlay -> noCmd <| setChatRoom
         {chatRoom | chatRoomOverlay =
           if isChatRoomOverlayOn chatRoom chatRoomOverlay
              then chatRoomOverlay else chatRoom.chatRoomOverlay}
       SetChatRoom cRoom -> noCmd <| setChatRoom cRoom
       SetMentionBox m ->
         (setChatRoom {chatRoom | mentionBox = m}
         ,Cmd.none --cmdMsg TriggerAutoScrollDown
         )
       AddMention str ->
         (setChatRoom {chatRoom
           | input = (String.fromList <| List.reverse <|
                      ListE.dropWhile (not << CharE.isSpace) <|
                      String.toList <| String.reverse chatRoom.input)
                     ++ str
           , mentionBox = Nothing}
         ,Cmd.none --cmdMsg TriggerAutoScrollDown
         )
       UpdateChatRoomInput message ->
         (setChatRoom {chatRoom
           | input = if String.contains "\n" message then "" else message
           , mentionBox = if lastWord message == "" then Nothing else chatRoom.mentionBox
           }
         ,cmdIf (String.contains "\n" message) <| cmdMsg <| BatchMsgs <|
           [SocketRequest <|
             UserMessageRequest chatLocale message
           ]
         )
       AddEmoteChatRoomInput str -> noCmd <| setChatRoom
         {chatRoom | input =
           if String.right 1 chatRoom.input == " " || chatRoom.input == ""
              then chatRoom.input ++ str ++ " "
              else chatRoom.input ++ " " ++ str ++ " "}




updateMessageRoom : UpdateElement MessageRoomMsg MessageRoom
updateMessageRoom model msg messageRoom liftMessageRoomtMsg setMessageRoom =
  let a = 1
  in case msg of
       MsgMessageRoomMsg msg_ -> pair model <| cmdMsg msg_
       SetHighlightUsersList highlightedUsers -> noCmd <| setMessageRoom <|
         {messageRoom | highlightedUsers = highlightedUsers}
       UpdateHighlightUsersList username -> noCmd <| setMessageRoom <|
         {messageRoom | highlightedUsers =
           let f names = case names of
                 [] -> [username]
                 name :: moreNames -> if username == name
                   then moreNames else name :: f moreNames
           in f messageRoom.highlightedUsers}
       -- event listeners stack with containers having priority
       -- el [Evt.onCLick] <- prioity
       --    el [Evt.onCLick] <- ignored
       -- SetHoverUsername is used to remove the top listener so the inner
       -- one can be used
       SetHoverUsername b -> noCmd <| setMessageRoom <|
         {messageRoom | hoverUsername = b}



--------------------------------------------------------------------------------

chatSetUp =
  []
