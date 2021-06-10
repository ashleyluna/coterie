module Update.ChatBox exposing (..)

import Char.Extra as CharE exposing (..)
import Dict
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListE exposing (..)
import String.Extra as StringE
import Task
import Time exposing (Posix, Zone)

import String.Extra exposing (clean)

import Internal.Chat exposing (..)
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




updateChatBox : UpdateElement ChatBoxMsg ChatBox
updateChatBox model msg chatBox liftChatBoxMsg setChatBox =
  let a = 1
  in case msg of
       MsgChatBoxMsg msg_ -> pair model <| cmdMsg msg_
       BatchChatBoxMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatBoxMsg) msgs
       ChatRoomMsg chatRoomMsg -> updateChatRoom model chatRoomMsg
         chatBox.chatRoom (liftChatBoxMsg << ChatRoomMsg)
         <| \newChatRoom -> setChatBox {chatBox | chatRoom = newChatRoom}
       WaitHalfASecChatBox message -> pair model <| do <|
         waitHalfASec <| Task.succeed <| liftChatBoxMsg message
       ChatBoxElmBarMsg elmBarMsg -> updateElmBar model
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
       POSTSignUpCheck strCheck -> pair model <|
         apiPOSTDefault "register/check" "check_username"
           [pair "username" <| JE.string strCheck] <|
           JD.map (liftChatBoxMsg << UpdateSignupCheck strCheck)
                  (JD.field "check_result" JD.int)
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
       POSTPronounsCheck str -> pair model <|
         apiPOSTDefault "profile" "check_pronouns"
           [pair "pronouns" <| JE.string str] <|
           flip JD.map (JD.field "check_result" JD.int) <| \int ->
             liftChatBoxMsg <| OverSettingsOverlay <| \info ->
                  if info.pronouns /= str then info
                     else  {info | invalidPronouns = int}
       POSTSetPronouns str -> pair model <|
         apiPOSTDefault "profile" "set_pronouns"
           [pair "pronouns" <| JE.string str] <|
           flip JD.map (JD.field "pronouns" <| JD.nullable JD.string) <| \newStr ->
             OverProfile <| \profile ->
               {profile | pronouns = newStr}
       POSTEquipBadge str int -> pair model <|
         apiPOSTDefault "profile" "equip_badge"
           [pair ("equip_" ++ String.fromInt int) <| JE.string str] <|
           flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             OverProfile <| setProfileBadges firstBadge secondBadge)
       POSTUnEquipBadge int -> pair model <|
         apiPOSTDefault "profile" ("unequip_badge_" ++ String.fromInt int)
           [] <|
           flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             OverProfile <| setProfileBadges firstBadge secondBadge)
       POSTSetDefaultColor maybeColor -> pair model <|
         apiPOSTDefault "profile" "set_default_color"
           [pair "default_color" <| jeMaybe jeDefaultNameColor maybeColor] <|
           flip JD.map jdDefaultNameColor (\c -> BatchMsgs
             [liftChatBoxMsg <| OverSettingsOverlay <| \info ->
               {info | defaultNameColor = maybeColor}
             ,OverProfile <| \profile ->
               let nameColor = profile.nameColor
               in {profile | nameColor =
                  {nameColor | defaultNameColor = c}}])
       POSTSetNameColor left right mode -> pair model <|
         apiPOSTDefault "profile" "set_name_color"
           [pair "left" <| jeChromaColor left
           ,pair "right" <| jeChromaColor right
           ,pair "mode" <| JE.string <| showChromaMode mode] <|
           JD.map3 (\left_ right_ mode_ -> BatchMsgs
             [OverProfile <| \profile -> {profile | nameColor =
               ProfileNameColorRecord profile.nameColor.defaultNameColor
                 left_ right_ mode_}
             ,liftChatBoxMsg <| OverSettingsOverlay <| \info ->
                {info | mode = mode_}])
             (JD.field "left" <| JD.nullable jdChromaColor)
             (JD.field "right" <| JD.nullable jdChromaColor)
             (JD.field "mode" jdChromaMode)
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
           [ChatRoomMsg <| MessageRoomMsg <| MessageRoomElmBarMsg <|
             GetElmBarViewport (str ++ "chatbox-chatroom-messageroom-elmbar") (AutoScrollDown False)]
           ++ let autoScroll = ChatBoxElmBarMsg << flip GetElmBarViewport (AutoScrollDown False)
              in case chatBox.chatBoxOverlay of
                   CommunityOverlay _ -> [autoScroll <| str ++ "chatbox-community-overlay"]
                   WhispersOverlay _ -> [autoScroll <| str ++ "chatbox-whispers-overlay"]
                   --SettingsOverlay _ -> [autoScroll <| str ++ "chatbox-settings-overlay"]
                   _ -> []


updateChatRoom : UpdateElement ChatRoomMsg ChatRoom
updateChatRoom model msg chatRoom liftChatRoomMsg setChatRoom =
  let a = 1
  in case msg of
       MsgChatRoomMsg msg_ -> pair model <| cmdMsg msg_
       BatchChatRoomMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatRoomMsg) msgs
       UserChatRoomMsgNow f -> pair model <| do <| Task.map (liftChatRoomMsg << f) Time.now
       MessageRoomMsg elmBarMsg -> updateMessageRoom model
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
       OverChatRoom f -> noCmd <| setChatRoom <| f chatRoom
       SetMentionBox m ->
         (setChatRoom {chatRoom | mentionBox = m}
         ,Cmd.none --cmdMsg TriggerAutoScrollDown
         )
       AddMention str ->
         (setChatRoom {chatRoom
           | input = (String.fromList <| List.reverse <|
                      ListE.dropWhile (not << CharE.isSpace) <|
                      String.toList <| String.reverse chatRoom.input)
                      ++ str ++ " "
           , mentionBox = Nothing}
         ,Cmd.none --cmdMsg TriggerAutoScrollDown
         )
       AddEmoteChatRoomInput str -> noCmd <| setChatRoom
         {chatRoom | input =
           if String.right 1 chatRoom.input == " " || chatRoom.input == ""
              then chatRoom.input ++ str ++ " "
              else chatRoom.input ++ " " ++ str ++ " "}
       SendUserMessage locale str time -> case model.commonInfo.profile of
         Nothing -> noCmd model
         Just profile ->
           let localeName = case locale of
                 MainChat -> [pair "type_chat" <| JE.string "main"]
                 ModChat -> [pair "type_chat" <| JE.string "mod"]
                 Whisper receiver -> [pair "type_chat" <| JE.string "whisper"
                                     ,pair "receiver" <| JE.string receiver]
           in pair model <| Cmd.batch
                [apiPOSTDefault "chat" "message"
                  ([pair "message" <| JE.string <| String.trim <| StringE.clean str
                   ] ++ localeName) <|
                  JD.succeed NoMsg
                ,cmdMsg <| case locale of
                  MainChat -> MainChatMsg <| AddTempUserMessage <| TempUserMessageRecord time <|
                    parseMessage model.commonInfo model.liveInfo.mainChat.users
                     (profileRenderOptions profile) str
                  _ -> ModChatMsg <| AddTempUserMessage <| TempUserMessageRecord time <|
                    parseMessage model.commonInfo model.liveInfo.modChat.users
                     (profileRenderOptions profile) str
                  --Whisper receiver -> Whisper receiver <| AddTempUserMessage message
               ]




updateMessageRoom : UpdateElement MessageRoomMsg MessageRoom
updateMessageRoom model msg messageRoom liftMessageRoomtMsg setMessageRoom =
  let a = 1
  in case msg of
       MsgMessageRoomMsg msg_ -> pair model <| cmdMsg msg_
       MessageRoomElmBarMsg msg_ -> updateElmBar model msg_ messageRoom.elmBar
         (liftMessageRoomtMsg << MessageRoomElmBarMsg)
         <| \newElmBar -> setMessageRoom {messageRoom | elmBar = newElmBar}
       NoHighlights -> noCmd <| setMessageRoom <|
         {messageRoom | highlightBox = Dict.empty}
       UpdateHighlightUsersList username -> if not <| Dict.member username messageRoom.highlightBox
         then (setMessageRoom <|
                {messageRoom | highlightBox = Dict.insert username Nothing messageRoom.highlightBox
                }
              ,apiPOSTDefault "mod" "get_user_info"
                 [pair "username" <| JE.string username] <|
                 JD.map (liftMessageRoomtMsg << AddUserInfo username) <| JD.map5 UserInfo
                   (JD.succeed username)
                   (JD.field "pronouns" JD.string)
                   (JD.field "num_months_subbed" JD.int)
                   (JD.field "season" JD.int)
                   (JD.maybe <| JD.map3 UserModInfo
                     (JD.field "account_creation" JD.int)
                     (JD.field "num_messages" JD.int)
                     (JD.field "mod_actions" <| JD.list <| jdModAction model.commonInfo)
                   )
              )
         else noCmd <| setMessageRoom {messageRoom | highlightBox =
                Dict.remove username messageRoom.highlightBox}
       AddUserInfo username userInfo -> noCmd <| setMessageRoom <|
         {messageRoom | highlightBox = flip Dict.map messageRoom.highlightBox <|
           \huUsername oldUserInfo -> if huUsername /= username then oldUserInfo else Just userInfo
         }
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
