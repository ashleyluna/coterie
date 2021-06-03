module Main.Model exposing (..)

import Array exposing (Array)
import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import Time exposing (Posix, Zone)
import Url exposing (Url)

import Browser.Navigation exposing (Key)


type Bool3 = Positive | Neutral | Negative
type Either3 a b c = Left a | Middle b | Right c

type alias Model =
  {setUp : SetUp
  ,commonInfo : CommonInfo -- low updates
  ,liveInfo : LiveInfo
  ,page : Page}


type alias CommonInfo =
  {localInfo : LocalInfo
  ,staticInfo : StaticInfo
  ,profile : Maybe ProfileRecord
  ,settings : SettingsRecord}


type alias LocalInfo =
  {url : Url
  ,key : Key
  ,zone : Maybe Time.Zone
  ,cookie : String
  }



type alias SetUp =
  {profile : Bool
  ,liveTime : Bool}


type Page = HomePage HomePageInfo
          | ChatPage ChatPageInfo
          | ChatStreamPage ChatStreamPageInfo
          | StreamerPage StreamerPageInfo
         -- | ModPage ModPageInfo
          | AdminPage --AdminPageInfo

          --| StreamChat StreamChatInfo




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


type alias StaticInfo =
  {specialRoles : Dict String Int
  ,specialRoleBadges : Dict String Emote
  ,subBadges : Array (List SubscriptionBadge)
  ,seasonBadges : List Emote
  ,badges : Dict String Emote
  ,globalEmoteList : Dict String Emote
  ,subOnlyEmoteList : Dict String Emote
  --,pointsRewards : Dict String Int --List PointsReward
  }

type alias SubscriptionBadge =
  {monthsRequired : Int
  ,emote : Emote}

type alias Emote =
  {name : String
  ,image : Image}

type alias Image =
  {width : Float
  ,height : Float
  ,url : String}

type alias PointsReward =
  {name : String
  ,cost : Int}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




type alias LiveInfo = -- argument = msg
  {streamStatus : Maybe StreamStatus
  --,shoutOutBox = List ShoutOutBoxCategory
  ,mainChat : Chat
  ,whispers : Maybe (List {whisperUser : ChatUser
                          ,whiserMessages : UserMessageRecord})
  ,modChat : Chat
  --,subGiftersLeaderBoard : List {username : String
  --                              ,numOfGifts : Int}
  }


type StreamStatus = Streaming StreamingRecord
                  | Hosting HostingRecord
                  | Offline

type alias StreamingRecord =
  {stream : StreamingService
  ,title : String
  ,startTime : Int -- start = stream started at 2 pm, Int = number of seconds in Posix
  ,upTime : Maybe Int
  ,viewerCount : Int} -- current time is 3pm, so upTime = 1 hour

type alias HostingRecord =
  {stream : StreamingService
  ,title : String}

type StreamingService = TwitchStream String
                      | YouTubeStream String




--type alias ShoutOutCategory =
--  {categoryName : String
--  ,links : List ShoutOutBoxElement
--  }
--
--type ShoutOutElement = ShoutOutLink (Int, String)
--                     | Creator (List ShoutOutElement)




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



type alias ProfileRecord =
  {account : AccountRecord
  ,username : String
  ,role : ProfileRole
  ,badges : ProfileBadges
  ,pronouns : Maybe String
  ,nameColor : ProfileNameColorRecord
  -- Data
  --,channelPoints : Int
  }

type alias AccountRecord =
  {email : String
  ,numMonthsSubbed : Int
  ,season : Int
  ,twitchConn : Bool
  ,googleConn : Bool
  }

type ProfileRole = ProfileSpecialRole ProfileSpecialRoleRecord
                 | ProfileChatterRole ProfileChatterRoleRecord

type alias ProfileSpecialRoleRecord =
  {name : String
  ,power : Int}

type alias ProfileChatterRoleRecord =
  {months : Int
  ,subscription : Maybe SubscriptionRecord}

type alias SubscriptionRecord =
  {tier : Int
  ,gifter : Maybe String
  ,is3MonthPackage : Bool
  ,recurring : Bool
  ,endTime : Int}

type alias ProfileBadges =
  {firstBadge : Maybe String
  ,secondBadge : Maybe String
  ,collection : List String}

type alias ProfileNameColorRecord =
  {defaultNameColor : Result DefaultNameColor DefaultNameColor
  ,left : Maybe ChromaColorRecord
  ,right : Maybe ChromaColorRecord
  ,mode : ChromaMode}

type DefaultNameColor
  = Blue
  | Azure
  | Sky
  | Turquoise
  | Turtle
  | Green
  | Slime
  | Yellow
  | Bronze
  | Mocha
  | Orange
  | DarkRed
  | Red
  | Rose
  | Pink
  | Purple
  | Lavender



{-

reddish - 4
greenish - 4
yellowish - 2
blueish - 3
purpleish - 2

r - red
b - blue
g - green
r - firebrick
r - coral
g - yellow green
r - ornge red
g - sea green
y - golden rod
y - chocolate
b - cadet blue
b - dodger blue
p - hot pink
p - blue violet
g - spring green

-}



type alias SettingsRecord =
  {themeMode : Bool
   -- Messages
  ,showTimestamps : Bool
  ,showEmotes : Bool
  ,animateEmotes : Bool -- TODO implement functionality
  ,showBadges : Bool
  ,styleUsernames : Maybe Bool -- TODO implement functionality
  ,textEmphasis : Maybe Bool -- Just True = Show, Just False = Raw, Nothing = Hide TODO implement functionality
  ,textSize : Int
  -- Filers
  ,mentionIgnoredUsers : Bool -- TODO implement functionality
  ,curseSwearWords : Bool -- TODO implement functionality
  ,nsfw : Bool -- TODO implement functionality
  ,showBannedMessages : Maybe Bool -- TODO implement functionality
  -- Other
  -- Allow Forced Stream/Chat Refreshes
  --auto complete helper
  }




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------





type alias Chat =
  {users : ChatUserList
  ,messages : List ChatMessage}

type alias ChatUserList =
  {specialUsers : List (Dict String (Dict String ChatUser)) -- order by rolePower, roleName, Username
  ,chatters : Dict String ChatUser}

type alias ChatUser =
  {username : String
  ,role : Maybe Role
  ,badges : List String
  ,pronouns : Maybe String
  ,nameColor : NameColor
  }

type NameColor = ChromaName ChromaColorRecord
               | ChromaNameGradient ChromaNameGradientRecord

type alias ChromaNameGradientRecord =
  {left : ChromaColorRecord
  ,right : ChromaColorRecord
  ,mode : ChromaMode}  -- lrgb, lch, hsl, etc.

type ChromaMode = LRGB
                | RGB
                | HSL
                | HSV
                | HSI
                | LCH
                | LAB

type alias ChromaColorRecord =
  {hue : Int
  ,chromaLight : Int -- light mode
  ,chromaDark : Int -- dark mode
  ,valueLight : Int
  ,valueDark : Int}

-- sub badge, emote access, color gradient access
type Role = Subscriber SubscriberRecord -- subscribers
          | Special String -- streamer, mods, vips

type alias SubscriberRecord =
  {tier : Int -- sub teir
  ,months : Int} -- number of months subbed

type ChatMessage = UserMessage UserMessageRecord
                 | SystemMessage SystemMessageRecord
                 --| SubNotification {username : String
                 --                  ,tier : SubTier
                 --                  ,months : Int}
                 --| GiftSubNotification {gifterUsername : Maybe String
                 --                      ,recieverUsername : String
                 --                      ,tier : SubTier}
                 | RawMessage RawMessageRecord -- for testing

type alias UserMessageRecord =
  {user : ChatUser
  ,time : Int -- in milli seconds
  --,messageType : Maybe SpecialMessage
  ,message : ParsedMessage
  }

type alias SystemMessageRecord =
  {time : Int
  ,message : ParsedMessage}
--type SpecialMessage = HightlightReward
type alias RawMessageRecord =
  {time : Int
  ,message : ParsedMessage}


type alias ParsedMessage =
  {unparsed : String
  ,parsed : List ParserdPiece}

type ParserdPiece = PText String
                  | PUserRef String String  -- actual username , username in message
                  | PEmote String
                 -- | PLink String -- url
                 -- | PItalics (List Message)  -- *...*
                 -- | PBold (List Message)     -- **...**
                 -- | PBoldItal (List Message) -- ***...***
                 -- | PStrike (List Message) -- __...__
                 -- | PColored (List Message)  -- |...|


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




type alias HomePageInfo = -- Nothing = Stream is the focus
                                -- Just a = reveal a (the contents of main box)
  {streamScreenSize : Bool -- True == Expand, False == Shrink
  ,subPage : HomeSubPage
  ,history : List HomeSubPage
  ,unappendHistory : Maybe HomeSubPage
  ,initiateSubPageSliding : Bool
  ,subPageSlideDirection : Bool -- True = left to right
  ,initiateSubPageHeaderSliding : Bool
  ,subPageHeaderSlideDirection : Bool -- True = left to right


  -- NavBar
  ,streamTitleLength : Float
  ,hoverTitle : Bool
  ,counterPosition : Int

  -- Register

  -- Settings

  -- Chat Box
  ,chatBox : ChatBox
  }

type HomeSubPage = HomeSubPageMain
                 | HomeSubPageSupport SupportRecord
                 | HomeSubPageDonate DonateRecord
                 | HomeSubPageSubscribe SubscribeRecord
                 | HomeSubPageSubscribe2 Subscribe2Record
                 | HomeSubPageAgreement

type alias MutualAidRecord =
  {headerPosition : Int
  }

type alias SupportRecord =
  {headerPosition : Int
  }

type alias DonateRecord =
    {name : String
    ,amount : String
    ,message : String}

type alias SubscribeRecord =
  {tier : Int
  ,initiateTierSliding : Bool
  ,tierSlideDirection : Bool
  ,recipient : Bool3
  ,autoRenew : Bool
  ,giftFriendSearch : String
  ,inValidGiftFriendSearch : Int
  ,numberOfRandomGiftSubs : Int
  }

-- subscription confirmation
type alias Subscribe2Record =
  {tier : Int
  ,recipient : Bool3
  ,autoRenew : Bool
  ,giftFriendSearch : String
  ,numberOfRandomGiftSubs : Int
  ,month3Package : Bool
  ,message : String
  }


--type AnimationState a = Still
--                    | StartAnimation a
--                    | AnimationInProcess a


type alias ChatPageInfo =
  {chatBox : ChatBox}

type alias ChatStreamPageInfo =
  {messageRoom : ElmBar MessageRoom}

type alias StreamerPageInfo =
  {chatBox : ChatBox
  ,modRoom : ChatRoom
  ,atMessageRoom : ElmBar MessageRoom
  ,streamStatus : Maybe Bool -- Nothing == Offline, Just False == Hosting, Just True == Streaming
  ,streamingTitle : String
  ,hostingSearch : String
  ,hostingCheck : Int
  }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------





type alias ChatBox =
  {chatRoom : ChatRoom
  ,elmBar : ElmBar ()
  ,chatBoxOverlay : ChatBoxOverlay
  --,chatBoxOverlayHistory : Maybe ChatBoxOverlay
  ,initiateChatBoxOverlayHeaderSliding : Bool
  ,chatBoxSlideDirection : Bool}

type ChatBoxOverlay = NoChatBoxOverlay
                    | ShoutOutBoxOverlay
                    | CommunityOverlay CommunityOverlayRecord

                    | PointsRewardsOverlay
                    | WhispersOverlay WhispersOverlayRecord
                    | SettingsOverlay SettingsOverlayRecord

                    | RegisterOverlay RegisterRecord

type alias CommunityOverlayRecord =
  {userSearch : String}
type alias WhispersOverlayRecord =
  {userSearch : String}

type alias SettingsOverlayRecord =
  {headerPosition : Int
  ,pronouns : String
  ,invalidPronouns : Int
  ,defaultNameColor : Maybe DefaultNameColor
  ,left : ChromaColorRecord
  ,right : ChromaColorRecord
  ,mode : ChromaMode
  }

type alias RegisterRecord =
  {remember : Bool
  ,signUp : Bool
  ,username : String
  ,invalidUsername : Int}
    -- ^ 0 = Good, 1 = Must Be AlphaNum,

type alias ChatRoom =
  {messageRoom : ElmBar MessageRoom
  ,chatRoomOverlay : ChatRoomOverlay
  ,mentionBox : Bool
  ,input : String}

type ChatRoomOverlay = NoChatRoomOverlay
                     | EmoteOverlay

type alias MessageRoom =
  {highlightedUsers : List String
  ,hoverUsername : Bool}

type alias HighlightedUsers = List String



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




--type ModPageInfo




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




--type AdminPageInfo




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


type alias ElmBar a =
  {viewport : Maybe Viewport
  ,infiniteScroll : Maybe
     {direction : Bool
     ,stack : Int}
  ,autoScroll : Maybe Bool
  ,content : a
  }
