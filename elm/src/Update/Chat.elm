module Update.Chat exposing (..)

import Dict exposing (Dict)
import Dict.Extra as DictE
import Json.Decode as JD
import Json.Encode as JE
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




updateChat : UpdateElement ChatMsg Chat
updateChat model msg chat liftChatMsg setChat =
  let a = 1 -- TODO remove if no let statements are needed
  in case msg of
       AddChatMessage message ->
         (setChat <| {chat | messages = if List.length chat.messages < 200
                               then chat.messages ++ [message]
                               else List.drop 1 chat.messages ++ [message]}
         ,cmdMsg <| BatchMsgs
           [TriggerAutoScrollDown
           --,
           ])
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
                _ -> {users | chatters = Dict.insert user.username user users.chatters}}
       RemoveChatUser username -> noCmd <| setChat <|
         {chat | users = let users = chat.users
           in {users | specialUsers =
                       flip List.map users.specialUsers <| Dict.map <| \_ ->
                         Dict.remove username
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
               Dict.map (\_ ls -> flip Dict.map (Dict.fromList ls) <|
                         \_ (user,_,_) -> user) <|
               DictE.groupBy (\(username,(user, power,roleName)) -> roleName) powerGroup
             chatters = Dict.filter (\_ user -> user.role == Nothing) userList
         in {chat | users = ChatUserList specialUsers chatters}
       AddSystemMessage liftChatMessage message -> pair model <| cmdMsg <|
         UseNow <| \now -> liftChatMessage <| AddChatMessage <| SystemMessage
           {time = Time.posixToMillis now
           ,message = parseMessage model.commonInfo model.liveInfo.mainChat.users
                                     True message}


--------------------------------------------------------------------------------

chatSetUp =
  []
