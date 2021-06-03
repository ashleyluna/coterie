module Main.Msg exposing (..)

import Browser.Dom as Dom exposing (Viewport)
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Time exposing (Posix, Zone)
import Url exposing (Url)

import Browser exposing (UrlRequest)

import Main.Model exposing (..)





type alias Update =
      Model -> Msg ->
      (Model, Cmd Msg) -> (Model, Cmd Msg)

type alias UpdatePage msg a =
     Model -> msg -> a
 -> (Model, Cmd Msg)

type alias UpdateElement msg a =
     Model -> msg -> a
  -> (msg -> Msg) -> (a -> Model)
  -> (Model, Cmd Msg)




type Msg
  = NoMsg
  | LogMessage String
  | BatchMsgs (List Msg)
  | MsgCmd (Cmd Msg)
  | WaitHalfASec Msg
  | UseNow (Posix -> Msg)
  | SetZone Time.Zone -- this should only be used at initialization
  | GetCookie
  | ReceiveCookie String
  | SetUpProfile
  | ApiREQUEST String Http.Body (List Http.Header) String String
               (JD.Decoder Msg) (Result Http.Error Msg -> Msg)
  | ApiGET (List Http.Header) String String
           (JD.Decoder Msg) (Result Http.Error Msg -> Msg)
  | ApiPOST (List Http.Header) String String
            (List (String, JE.Value)) (JD.Decoder Msg) (Result Http.Error Msg -> Msg)
  --| SetUp -- this should on be used with initialization
  | RequestUrl UrlRequest
  | ChangeUrl Url
  | SocketRequest SocketRequest
  | SocketResponse String




  -- Server Info
  | UpdateStreamStatus StreamStatus
  | ChangeStreamStatus (Maybe (Result String String))
  | UpdateLiveTime Int
  | MainChatMsg ChatMsg
  | ModChatMsg ChatMsg
  -- Shout Out Box




  -- Main User
  | GETUserMe
  | GETLogOut
  | POSTGoogleSignIn JD.Value
  | StartTwitchSignIn
  | SetProfile (Maybe ProfileRecord)
  | OverProfile (ProfileRecord -> ProfileRecord)
  | SetSettings SettingsRecord
  --| GETUserMe
  --| GOTUserMe (Result Http.Error ProfileRecord)


  -- Psuedo PageMSG
  | TriggerAutoScrollDown

  | SetPage Page
  | HomePageMsg HomePageMsg
  | ChatPageMsg ChatPageMsg
  | ChatStreamPageMsg ChatStreamPageMsg
  | StreamerPageMsg StreamerPageMsg




type ChatMsg = AddChatMessage ChatMessage
             | RemoveUserMessage String String -- username message
             | AddChatUser ChatUser
             | RemoveChatUser String -- username
             | CensorChatUser String -- username
             | SetUserList (Dict String ChatUser)
             | AddSystemMessage (ChatMsg -> Msg) String
--type PageMsg = HomePageMsg HomePageMsg

--------------------------------------------------------------------------------
-- Pages

type HomePageMsg =
    SetStreamScreenSize Bool
  | UpdateSubPage HomeSubPage
  | AppendSubPage HomeSubPage
  | UnappendSubPage
  | SetSubPageDefault Bool
  | InitiateSubPageSlide Bool
  -- ^ this bool if for slide direction, think True == Remove, False == Add on
  | TurnOffSubPageSlide
  | SlideSubPageHeader Bool Bool HomeSubPage

  -- NavBar
  | CheckStreamTitleLength
  | ChangeStreamTitleLength Float
  | HoverTitle Bool
  | NextCounterPosition

  --| ChatRoomMessageRecieverDecoder String
  --| AddEmoteChatInput String

  | HomeChatBoxMsg ChatBoxMsg

  | HomeTestMsg HomeSubPage


  -- API Messages

  -- Subscribe
  | GiftFriendCheck String
  | POSTGiftFriendCheck String
  | UpdateGiftFriendCheck String Int
  | POSTSubscription Subscribe2Record
  | POSTDonation DonateRecord


type ChatPageMsg = ChatPageChatBoxMsg ChatBoxMsg

type ChatStreamPageMsg = ChatStreamPageMessageRoomMsg (ElmBarMsg MessageRoomMsg)

type StreamerPageMsg = StreamerPageChatBoxMsg ChatBoxMsg
                     | StreamerPageModRoomMsg ChatRoomMsg
                     | StreamerPageMentionMessageRoomMsg (ElmBarMsg MessageRoomMsg)

                     | OverStreamerPage (StreamerPageInfo -> StreamerPageInfo)
                     | StreamStatusSetter (Maybe Bool)
                     | HostCheck String

--------------------------------------------------------------------------------
-- ChatBox Segments

type ChatBoxMsg = ChatBoxMsg Msg
                | BatchChatBoxMsgs (List ChatBoxMsg)
                | WaitHalfASecChatBox (ChatBoxMsg)

                | SetChatBoxOverlay ChatBoxOverlay
                | UpdateChatBoxOverlay ChatBoxOverlay
                | OverSettingsOverlay (SettingsOverlayRecord -> SettingsOverlayRecord)
                | OverCommunityOverlay (CommunityOverlayRecord -> CommunityOverlayRecord)
                | SlideChatBoxOverlayHeader Bool Bool ChatBoxOverlay
                | ChatRoomMsg ChatRoomMsg

                --Apit Messages
                --| Register
                | SignUpCheck String
                | POSTSignUpCheck String
                | UpdateSignupCheck String Int
                | StartGoogleSignIn
                --| POSTSignUp
                --    {email : String
                --    ,username : String
                --    ,password : String}
                -- Settings
                | PronounsCheck String
                | POSTPronounsCheck String
                | POSTSetPronouns String
                | POSTEquipBadge String Int
                | POSTUnEquipBadge Int
                | POSTSetDefaultColor (Maybe DefaultNameColor)
                | POSTSetNameColor ChromaColorRecord ChromaColorRecord ChromaMode
               -- | POSTSetMode ChromaMode

               | ChatBoxElmBarMsg (ElmBarMsg ChatBoxMsg)
               | ChatBoxTriggerAutoScrollDown String

type ChatRoomMsg = MessageRoomMsg (ElmBarMsg MessageRoomMsg)
                 | SetChatRoomOverlay ChatRoomOverlay
                 | UpdateChatRoomOverlay ChatRoomOverlay
                 | UpdateChatRoomInput String
                 | SetMentionBox Bool
                 | AddEmoteChatRoomInput String

type MessageRoomMsg = SetHighlightUsersList (List String)
                    | UpdateHighlightUsersList String
                    | SetHoverUsername Bool
                   -- auto scroll stuff




--------------------------------------------------------------------------------




type SocketRequest
  = GESubscription
      {mainChat : Bool
      ,streamStatus : Bool
      ,mod : Bool
      }
  | GetStreamStatus
  | GetUsersInChat
  | UserMessageRequest ChatLocale String


type ChatLocale
  = MainChat
  | ModChat
  | Whisper String





type ElmBarMsg msg = ElmBarMsg msg
                   | BatchElmBarMsgs (List (ElmBarMsg msg))
                   | GetElmBarViewport String (String -> Viewport -> ElmBarMsg msg)
                   | SetElmBarViewPort Viewport
                   | OnScroll String Viewport
                   | ResetScrollStack
                   | AutoScrollDown Bool String Viewport
                   | ElmBarWait (ElmBarMsg msg)
