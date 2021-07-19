module Update.Chat exposing (..)

import Dict exposing (Dict)
import Dict.Extra as DictE
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListE exposing (..)
import Time exposing (Posix, Zone)

import Internal.Chat exposing (..)
import Internal.Internal exposing (..)
import Main.Model exposing (..)
import Main.Msg exposing (..)
import Main.Ports exposing (..)

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




--updateChat : UpdateElement ChatMsg Chat
updateChat model msg chat liftChatMsg setChat =
  let appendMessage message = if List.length chat.messages < 200
                        then chat.messages ++ [message]
                        else List.drop 1 chat.messages ++ [message]
  in case msg of
       AddChatMessage message ->
         (setChat <| {chat | messages = case Debug.log "AddChatMessage" message of
           UserMessage userMessage ->
             let isSameMessage message_ = case message_ of
                   DelayedUserMessage delayedMessage ->
                     userMessage.user.username == userMessage.user.username
                     && userMessage.message.unparsed == delayedMessage.message.unparsed
                   TempUserMessage tempUserMessage ->
                     Just userMessage.user.username == Maybe.map .username model.commonInfo.profile
                     && userMessage.message.unparsed == tempUserMessage.message.unparsed
                   _ -> False
             in case ListE.findIndex isSameMessage chat.messages of
                  Just int -> ListE.setAt int (UserMessage userMessage) chat.messages
                  _ -> appendMessage message
           _ -> appendMessage message
           }
         ,cmdMsg <| Msg TriggerAutoScrollDown)
       AddDelatedUserMessage userMessage ->
         (setChat <| {chat | messages = appendMessage <| DelayedUserMessage userMessage}
         ,cmdMsg <| Msg TriggerAutoScrollDown)
       AddTempUserMessage message ->
         (setChat <| {chat | messages = appendMessage <| TempUserMessage message}
         ,cmdMsg <| Msg TriggerAutoScrollDown)
       RemoveUserMessage username str -> noCmd <| setChat <|
         let removeMessage messages = case messages of -- slightly more efficient than filter
               message2 :: otherMessages -> case message2 of
                 UserMessage userM -> if userM.user.username == username
                                      && userM.message.unparsed == str
                   then otherMessages else message2 :: removeMessage otherMessages
                 _ -> message2 :: removeMessage otherMessages
               _ -> []
         in {chat | messages = removeMessage chat.messages}
       AddChatUser user -> noCmd <| setChat <|
         {chat | users = let users = chat.users
           in case user.role of
                Just (Special str) -> {users | specialUsers =
                  flip List.map users.specialUsers <| Dict.update str <| Maybe.map <|
                    Dict.insert user.username user}
                Just (Subscriber sub) -> {users | subscribers =
                  ListE.updateAt (sub.tier - 1) (Dict.insert user.username user) users.subscribers}
                _ -> {users | chatters = Dict.insert user.username user users.chatters}}
       RemoveChatUser username -> noCmd <| setChat <|
         {chat | users = let users = chat.users
           in {users | specialUsers = flip List.map users.specialUsers <|
                         Dict.map <| \_ -> Dict.remove username
                     , subscribers = List.map (Dict.remove username) users.subscribers
                     , chatters = Dict.remove username users.chatters}}
         --{chat | users = Dict.remove username chat.users}
       CensorChatUser username -> noCmd <| setChat <|
         {chat | messages = flip List.filter chat.messages <| \message ->
           case message of
             UserMessage userMessage -> userMessage.user.username /= username
             _ -> True}
       SetUserList userList -> noCmd <| setChat <|
         let specialRoles = model.commonInfo.staticInfo.specialRoles

             dictByName : Dict String (ChatUser, Int, String) -- username, (user, power, roleName)
             dictByName = flip DictE.filterMap userList <| \name user -> case user.role of
               Just (Special str) -> Maybe.map (\int -> (user, int, str)) <| Dict.get str specialRoles
               _ -> Nothing
             groupsByPower : List (List (String, (ChatUser, Int, String))) -- List By RolePower (List of (username, (nameColor, power, roleName)))
             groupsByPower = Dict.values <| flip DictE.groupBy (Dict.toList dictByName) <|
               \(username,(user, power,roleName)) -> power
             specialUsers : List (Dict String (Dict String ChatUser)) -- List By RolePower (Dict By RoleName (List (username, (nameColor, power, roleName))))
             specialUsers = flip List.map groupsByPower <| \powerGroup ->
               Dict.map (\_ ls -> flip Dict.map (Dict.fromList ls) <| \_ (user,_,_) -> user) <|
               DictE.groupBy (\(username,(user, power,roleName)) -> roleName) powerGroup
               --flip DictE.filterMap userList <| \name user -> case user.role of
               -- TODO no idea what ^ code was suppose to be, subscribers need to ordered later
             chatters = Dict.filter (\_ user -> user.role == Nothing) userList
         in {chat | users = ChatUserList specialUsers [] chatters}
       AddSystemMessage liftChatMessage message -> pair model <| cmdMsg <|
         UseNow <| \now -> liftChatMessage <| AddChatMessage <| SystemMessage
           {timestamp = Time.posixToMillis now
           ,message = parseMessage model.commonInfo model.liveInfo.mainChat.users
                                     renderAll message}


--------------------------------------------------------------------------------

chatSetUp =
  []
