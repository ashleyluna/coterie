module Update.ChatBox exposing (..)

import Browser.Dom as Dom exposing (Viewport)
import Char.Extra as CharE exposing (..)
import Dict
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListE exposing (..)
import Result.Extra as ResultE
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




updateChatBox chatBox setChatBox model msg liftChatBoxMsg =
  updateGeneralMsg model msg liftChatBoxMsg <|
    updateChatBox_ chatBox setChatBox
--updateChatBox : UpdateElement ChatBoxMsg ChatBox
updateChatBox_ chatBox setChatBox model msg liftChatBoxMsg =
  let a = 1
  in case msg of
       --MsgChatBoxMsg msg_ -> pair model <| cmdMsg msg_
       --BatchChatBoxMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatBoxMsg) msgs
       ChatRoomMsg chatRoomMsg -> updateChatRoom
         chatBox.chatRoom (\newChatRoom -> setChatBox {chatBox | chatRoom = newChatRoom})
         model chatRoomMsg (liftChatBoxMsg << Msg << ChatRoomMsg)
       --WaitHalfASecChatBox message -> pair model <| do <|
       --  waitHalfASec <| Task.succeed <| liftChatBoxMsg message
       ChatBoxElmBarMsg elmBarMsg -> updateElmBar
         chatBox.elmBar (\newElmBar -> setChatBox {chatBox | elmBar = newElmBar})
         liftChatBoxMsg
         model elmBarMsg (liftChatBoxMsg << Msg << ChatBoxElmBarMsg)


       SetChatBoxOverlay chatBoxOverlay ->
         (setChatBox {chatBox | chatBoxOverlay =
           if isChatBoxOverlayOn chatBox chatBoxOverlay
              then NoChatBoxOverlay else chatBoxOverlay}
         ,cmdMsg <| liftChatBoxMsg <| BatchMsgs
            [Msg <| ChatRoomMsg <| Msg <| SetChatRoomOverlay NoChatRoomOverlay
            ,Msg <| ChatRoomMsg <| Msg <| SetMentionBox Nothing
            --,ChatBoxElmBarMsg ResetScrollStack
            ])
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
                    <| Msg <| SlideChatBoxOverlayHeader False direction overlayInfo
         else noCmd <| setChatBox
                {chatBox | initiateChatBoxOverlayHeaderSliding = False}


       -- Register
       SignUpCheck strCheck -> pair model
         <| case chatBox.chatBoxOverlay of
              RegisterOverlay info -> if info.username == strCheck
                then do <| Task.succeed <| liftChatBoxMsg <| Msg <|
                           POSTSignUpCheck strCheck
                else Cmd.none
              _ -> Cmd.none
       POSTSignUpCheck strCheck -> pair model <|
         apiPOSTDefault "register/check" "check_username"
           [pair "username" <| JE.string strCheck] <|
           JD.map (liftChatBoxMsg << Msg << UpdateSignupCheck strCheck)
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
             then cmdMsg <| liftChatBoxMsg <| Msg <| POSTPronounsCheck str
             else Cmd.none
           _ -> Cmd.none
       POSTPronounsCheck str -> pair model <|
         apiPOSTDefault "profile" "check_pronouns"
           [pair "pronouns" <| JE.string str] <|
           flip JD.map (JD.field "check_result" JD.int) <| \int ->
             liftChatBoxMsg <| Msg <| OverSettingsOverlay <| \info ->
                  if info.pronouns /= str then info
                     else  {info | invalidPronouns = int}
       POSTSetPronouns str -> pair model <|
         apiPOSTDefault "profile" "set_pronouns"
           [pair "pronouns" <| JE.string str] <|
           flip JD.map (JD.field "pronouns" <| JD.nullable JD.string) <| \newStr ->
             Msg <| OverProfile <| \profile ->
               {profile | pronouns = newStr}
       POSTEquipBadge str int -> pair model <|
         apiPOSTDefault "profile" "equip_badge"
           [pair ("equip_" ++ String.fromInt int) <| JE.string str] <|
           flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             Msg <| OverProfile <| setProfileBadges firstBadge secondBadge)
       POSTUnEquipBadge int -> pair model <|
         apiPOSTDefault "profile" ("unequip_badge_" ++ String.fromInt int)
           [] <|
           flip JD.map jdEquipedBadges <| (\(firstBadge, secondBadge) ->
             Msg <| OverProfile <| setProfileBadges firstBadge secondBadge)
       POSTSetDefaultColor maybeColor -> pair model <|
         apiPOSTDefault "profile" "set_default_color"
           [pair "default_color" <| jeMaybe jeDefaultNameColor maybeColor] <|
           flip JD.map jdDefaultNameColor (\c -> BatchMsgs
             [liftChatBoxMsg <| Msg <| OverSettingsOverlay <| \info ->
               {info | defaultNameColor = maybeColor}
             ,Msg <| OverProfile <| \profile ->
               let nameColor = profile.nameColor
               in {profile | nameColor =
                  {nameColor | defaultNameColor = c}}])
       POSTSetNameColor left right mode -> pair model <|
         apiPOSTDefault "profile" "set_name_color"
           [pair "left" <| jeChromaColor left
           ,pair "right" <| jeChromaColor right
           ,pair "mode" <| JE.string <| showChromaMode mode] <|
           JD.map3 (\left_ right_ mode_ -> BatchMsgs
             [Msg <| OverProfile <| \profile -> {profile | nameColor =
               ProfileNameColorRecord profile.nameColor.defaultNameColor
                 left_ right_ mode_}
             ,liftChatBoxMsg <| Msg <| OverSettingsOverlay <| \info ->
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
         liftChatBoxMsg <| BatchMsgs <|
           [Msg <| ChatRoomMsg <| Msg <| MessageBoxMsg <| Msg <| getViewportAndAutoScroll
             (MessageBoxElmBarMsg) (str ++ "chatbox-chatroom-messageroom-elmbar")]
           ++ let autoScroll = Msg << getViewportAndAutoScroll ChatBoxElmBarMsg
              in case chatBox.chatBoxOverlay of
                   CommunityOverlay _ -> [autoScroll <| str ++ "chatbox-community-overlay"]
                   WhispersOverlay _ -> [autoScroll <| str ++ "chatbox-whispers-overlay"]
                   --SettingsOverlay _ -> [autoScroll <| str ++ "chatbox-settings-overlay"]
                   _ -> []


updateChatRoom chatRoom setChatRoom model msg liftChatRoomMsg =
  updateGeneralMsg model msg liftChatRoomMsg <|
    updateChatRoom_ chatRoom setChatRoom
--updateChatRoom : UpdateElement ChatRoomMsg ChatRoom
updateChatRoom_ chatRoom setChatRoom model msg liftChatRoomMsg =
  let a = 1
  in case msg of
       --MsgChatRoomMsg msg_ -> pair model <| cmdMsg msg_
       --BatchChatRoomMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftChatRoomMsg) msgs
       UserChatRoomMsgNow f -> pair model <| do <| Task.map (liftChatRoomMsg << f) Time.now
       MessageBoxMsg msg_ -> updateMessageBox
         chatRoom.messageBox (\newMessageBox -> setChatRoom {chatRoom | messageBox = newMessageBox})
         model msg_ (liftChatRoomMsg << Msg << MessageBoxMsg)
       --MessageBoxMsg messageBoxMsg -> updateMessageBox model messageBoxMsg
       --  chatRoom.messageBox (liftChatRoomMsg << MessageBoxMsg)
       --  <| \newMessageBox -> setChatRoom {chatRoom | messageBox = newMessageBox}

       SetChatRoomOverlay chatRoomOverlay ->
         (setChatRoom
           {chatRoom | chatRoomOverlay =
             if isChatRoomOverlayOn chatRoom chatRoomOverlay
                then NoChatRoomOverlay else chatRoomOverlay}
           ,cmdMsg <| liftChatRoomMsg <| Msg <| SetMentionBox Nothing)
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
       SendUserMessage locale str timestamp -> case model.commonInfo.profile of
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
                ,cmdMsg <| Msg <| case locale of
                  MainChat -> MainChatMsg <| AddTempUserMessage <| TempUserMessageRecord timestamp <|
                    parseMessage model.commonInfo model.liveInfo.mainChat.users
                     (profileRenderOptions profile) str
                  _ -> ModChatMsg <| AddTempUserMessage <| TempUserMessageRecord timestamp <|
                    parseMessage model.commonInfo model.liveInfo.modChat.users
                     (profileRenderOptions profile) str
                  --Whisper receiver -> Whisper receiver <| AddTempUserMessage message
               ]



updateMessageBox messageBox setMessageBox model msg liftMessageBoxMsg =
  updateGeneralMsg model msg liftMessageBoxMsg <|
    updateMessageBox_ messageBox setMessageBox
--updateMessageBox_ : UpdateElement MessageBoxMsg_ MessageBox
updateMessageBox_ messageBox setMessageBox model msg liftMessageBoxMsg =
  let a = 1
  in case msg of
       --MsgMessageBoxMsg msg_ -> pair model <| cmdMsg msg_
       --BatchMessageBoxMsgs msgs -> pair model <| Cmd.batch <| List.map (cmdMsg << liftMessageBoxMsg) msgs
       OverMessageBox f -> noCmd <| setMessageBox <| f messageBox
       MessageBoxElmBarMsg msg_ -> updateElmBar
         messageBox.elmBar (\newElmBar -> setMessageBox {messageBox | elmBar = newElmBar})
         liftMessageBoxMsg
         model msg_ (liftMessageBoxMsg << Msg << MessageBoxElmBarMsg)
       HighlightBoxElmBarMsg msg_ -> case messageBox.tray of
         Nothing -> noCmd model
         Just tray -> updateElmBar
           tray.elmBar (\newElmBar -> setMessageBox {messageBox | tray = Just {tray | elmBar = newElmBar}})
           liftMessageBoxMsg
           model msg_ (liftMessageBoxMsg << Msg << HighlightBoxElmBarMsg)
       -- event listeners stack with containers having priority
       -- el [Evt.onCLick] <- prioity
       --    el [Evt.onCLick] <- ignored
       -- SetHoverUsername is used to remove the top listener so the inner
       -- one can be used
       SetHoverUsername b -> noCmd <| setMessageBox <|
         {messageBox | hoverUsername = b}




       --
       CloseHighlightBox -> noCmd <| setMessageBox <|
         {messageBox | userBarSelected = 1
                     , userBarMargin = 0
                     , tray = Nothing
                     , highlightList = []}
       SwitchHighlightUser containerName username -> pair model <| cmdMsg <| liftMessageBoxMsg <| Msg <|
         case ListE.findIndex (\u -> u.username == username) messageBox.highlightList of
           Just int -> SetHighlightedUserSelected containerName int
           _ -> UpdateHighlightUsersList containerName username
       SetHighlightedUserSelected containerName int ->
         (setMessageBox <| {messageBox | userBarSelected = int}
         ,flip Task.attempt
           (Task.map4 (\container wrapper el0 element -> SetUserBarMargin <|
             max 0 <| min (wrapper.scene.width - container.viewport.width)
                          (element.element.x - el0.element.x))
             (Dom.getViewportOf <| containerName ++ "userbar-container")
             (Dom.getViewportOf <| containerName ++ "userbar-wrapper")
             (Dom.getElement <| containerName ++ "userbar-" ++ String.fromInt 1)
             (Dom.getElement <| containerName ++ "userbar-" ++ String.fromInt (max 1 <| int)))
           <| \res -> case res of
             Ok msg_ -> liftMessageBoxMsg <| Msg msg_ --LogMessage <| "success: " ++ String.fromInt int ++ ", " ++ String.fromFloat x
             Err (Dom.NotFound err) -> LogMessage <| "Not found: " ++ err
         )
       SetUserBarMargin int -> noCmd <| setMessageBox <|
         {messageBox | userBarMargin = int}
       POSTBanUser username -> pair model <|
         apiPOSTDefault "mod" "ban"
           [pair "username" <| JE.string username] <|
           JD.succeed NoMsg
       POSTCensorUser username int -> pair model <|
         apiPOSTDefault "mod" "censor"
           [pair "username" <| JE.string username
           ,pair "time" <| JE.int int] <|
           JD.succeed NoMsg




       OpenTray int -> case int of
         _ -> let -- highlighted users that dont have userinfo
                  usernameList = flip List.concatMap messageBox.highlightList <| \user ->
                    mapMaybeToList user.userInfo <| \userInfo ->
                    mapMaybeToList userInfo.modInfo <| \modInfo ->
                    case modInfo.messagesInfo of
                      Nothing -> [user.username]
                      _ -> []
              in (setMessageBox
                    {messageBox | tray = Just
                      {open = int
                      ,makingRequest = usernameList /= []
                      ,focus = Nothing
                      ,elmBar = {viewport = Nothing
                                --,infiniteScroll = Nothing
                                ,autoScroll = Just True}
                      }}
                 ,cmdIf (usernameList /= []) <|
                   cmdMsg <| liftMessageBoxMsg <| Msg <|
                     POSTRequestUserMessageHistory <| Err usernameList)
       CloseTray -> noCmd <| setMessageBox
         {messageBox | tray = Nothing}
       UpdateHighlightUsersList containerName username ->
         let setUserSelected = cmdMsg << liftMessageBoxMsg << Msg << SetHighlightedUserSelected containerName
         in case (ListE.findIndex (\u -> u.username == username) messageBox.highlightList, List.length messageBox.highlightList) of
              (Just int, 1) -> pair model <| cmdMsg <| liftMessageBoxMsg <| Msg CloseHighlightBox
                -- remove user from highlightlist
              (Just int, _) -> (setMessageBox <|
                  {messageBox | highlightList = ListE.removeAt (int - 1) messageBox.highlightList}
                ,setUserSelected <| max 1 <| messageBox.userBarSelected - 1)
              -- add user to highlightlist
              _ -> (setMessageBox <|
                     {messageBox | highlightList = messageBox.highlightList
                                                ++ [HighlightedUser username Nothing]}
                   ,Cmd.batch
                     [setUserSelected <| List.length messageBox.highlightList + 1
                     ,apiPOSTDefault "chat" "get_user_info"
                      [pair "username" <| JE.string username] <|
                      JD.map (liftMessageBoxMsg << Msg << AddHighlightedUserInfo username) <|
                        JD.map identity (JD.map8 HighlightedUserInfo
                          (JD.field "role" <| JD.nullable jdSimpleRole)
                          (JD.field "badges" <| JD.list JD.string)
                          (JD.field "pronouns" <| JD.nullable JD.string)
                          (JD.field "name_color" jdNameColor_)
                          (JD.field "account_creation" JD.int)
                          (JD.field "power" JD.int)
                          (JD.field "season" JD.int)
                          (JD.maybe <| JD.map2 HighlightedUserModInfo
                            (JD.field "meaningful_messages" JD.int)
                            (JD.succeed Nothing)))
                     ]
                   )
       AddHighlightedUserInfo username userInfo -> noCmd <| setMessageBox <|
        {messageBox | highlightList = flip List.map messageBox.highlightList <|
          \user -> if user.username /= username then user else {user | userInfo = Just userInfo}
        }
       OnScrollHighlightedMessages tray viewport topMessageRequest bottomMessageRequest ->
         if not tray.makingRequest
            then if 100 >= viewport.viewport.y
                    then pair model <| cmdMsg <| liftMessageBoxMsg topMessageRequest
                 else if 100 >= viewportMarginDown viewport
                    then pair model <| cmdMsg <| liftMessageBoxMsg bottomMessageRequest
                    else noCmd model
            else noCmd model
       -- POSTRequestUserMessages asks for mesasges sent to main chat.
       -- Nothing = recent 50 messages. Just int = ask for 50 before the time of int
       POSTRequestUserMessageHistory reqInfo -> pair model <|
         apiPOSTDefault "mod" "user_messages"
           (case reqInfo of
             -- initial user messages
             Err usernames -> [pair "usernames" <| JE.list JE.string usernames]
             -- messages some time in the past relative to a timestamp for each user
             -- If past then ask for messages before the timestamps, otherwise messages after
             Ok (past, usersInfo) ->
               [pair "users" <| flip JE.list usersInfo <|
                 \(username, timestamp) -> JE.object
                   [pair "username" <| JE.string username
                   ,pair "timestamp" <| JE.int timestamp]
               ,pair "past" <| JE.bool past]
           ) <|
           JD.field "users" <|
             JD.map (liftMessageBoxMsg << Msg << AddHighlightedUsersMessages (ResultE.unwrap False Tuple.first reqInfo)) <|
               JD.list <| JD.map3 triple
                (JD.field "username" JD.string)
                (JD.field "end" JD.bool)
                (JD.field "messages" <| JD.list <| JD.map2 HighlightedMessageRecord
                  (JD.field "timestamp" JD.int)
                  (JD.field "message" <| flip JD.map JD.string <|
                    parseMessage model.commonInfo
                                 model.liveInfo.mainChat.users
                                 (RenderOptions True False))
                )
       AddHighlightedUsersMessages past usersInfo -> noCmd <| setMessageBox <|
         {messageBox | highlightList =
           let addInfo highlightUser (username, end, messages) =
                 if highlightUser.username /= username then highlightUser
                    else {highlightUser | userInfo = flip Maybe.map highlightUser.userInfo <|
                           \userInfo -> {userInfo | modInfo = flip Maybe.map userInfo.modInfo <|
                           \modInfo -> {modInfo | messagesInfo = Just <|
                             let topEndTimestamp = if end then Nothing else
                                   Maybe.map .timestamp <| List.head <| List.reverse messages
                                 bottomEndTimestamp = if end then Nothing else
                                   Maybe.map .timestamp <| List.head messages
                             in case modInfo.messagesInfo of
                             Nothing -> {top = if past then Nothing else topEndTimestamp
                                        ,bottom = if not past then Nothing else bottomEndTimestamp
                                        ,messages = messages}
                             Just messagesInfo -> if past
                               then {messagesInfo | top = topEndTimestamp, messages = messages ++ messagesInfo.messages}
                               else {messagesInfo | bottom = bottomEndTimestamp, messages = messagesInfo.messages ++ messages}
                           }}}
           in List.map2 addInfo messageBox.highlightList usersInfo
         }



--------------------------------------------------------------------------------

chatSetUp =
  []
