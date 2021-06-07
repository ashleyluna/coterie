module Internal.Internal exposing (..)

import Char.Extra as CharE exposing (..)
import Html.Attributes as HtmlA
import Http
import Json.Encode as JE
import List.Extra as ListE exposing (..)
import Parser exposing ((|.),(|=))
import Process
import Task
import Tuple
import Url exposing (..)

import Element exposing (..)

import Main.Msg exposing (..)
import Main.Model exposing (..)





pair = Tuple.pair


wrapIf b f =
  if b then f else identity

listIf b ls = if b then ls else []

cmdIf b c = if b then c else Cmd.none

flip : (a -> b -> c) -> (b -> a -> c)
flip f b a = f a b
flip2 : (a -> b -> c -> d) -> (b -> c -> a -> d)
flip2 f b c a = f a b c

-- Cmd

--do : Task.Task msg -> Cmd msg
do = Task.perform identity

cmdMsg : msg -> Cmd msg
cmdMsg = do << Task.succeed

noCmd : model -> (model, Cmd msg)
noCmd model = (model, Cmd.none)

-- render at least 1 frame before changing the state
-- 30 milliseconds (1 / 33 frames per second) should
-- be a safe bet
wait n task = Task.andThen (\_ -> task) <| Process.sleep n
waitAFrame task = Task.andThen (\_ -> task) <| Process.sleep 30
waitHalfASec task = Task.andThen (\_ -> task) <| Process.sleep 500


subCost : Int -> Int -> (Int, Int, Int)
subCost tier num = -- num is in case of gifts
  (           5 *         (2 ^ (tier - 1)) *         num      -- 1 month cost
  ,round <| 5.0 * toFloat (2 ^ (tier - 1)) * toFloat num  * 2.4 -- 3 month cost
  ,           5 *         (2 ^ (tier - 1)) *         num  * 3   -- without -20%
  )

showNumMetric : Int -> String
showNumMetric n = if n < 1000 then String.fromInt n
                  else if n < 1000000 then String.fromInt (n // 1000) ++ "." ++ String.left 1 (String.fromInt (remainderBy 1000 n)) ++ "K"
                  else String.fromInt (n // 1000000) ++ "." ++ String.left 2 (String.fromInt (remainderBy 1000000 n)) ++ "M"


lastWord str = String.fromList <| List.reverse <|
               ListE.takeWhile (not << CharE.isSpace) <|
               String.toList <| String.reverse str


apiUrl url =
  (if url.protocol == Http then "http" else "https")
  ++ "://"
  ++ url.host
  ++ Maybe.withDefault "" (Maybe.map ((++) ":" << String.fromInt) url.port_)
  ++ "/api/"


httpErrorMsg err = case err of
  Http.BadUrl str -> LogMessage <| "BadUrl: " ++ str
  Http.Timeout -> LogMessage "Timeout"
  Http.NetworkError -> LogMessage "NetworkError"
  Http.BadStatus int -> LogMessage <| "BadStatus: " ++ String.fromInt int
  Http.BadBody str -> LogMessage <| "BadBody: " ++ str


showChromaMode mode = case mode of
  LRGB -> "LRGB"
  RGB -> "RGB"
  HSL -> "HSL"
  HSV -> "HSV"
  HSI -> "HSI"
  LCH -> "LCH"
  LAB -> "LAB"


defaultChromaColor : DefaultNameColor -> ChromaColorRecord
defaultChromaColor color = case color of
  Blue  -> ChromaColorRecord 294 100 100 33 43
  Azure      -> ChromaColorRecord 260 96  96  48 58
  Sky       -> ChromaColorRecord 237 130 130 66 76

  Turquoise -> ChromaColorRecord 179 74  74  65 75
  Turtle    -> ChromaColorRecord 170 102 102 37 47
  Green     -> ChromaColorRecord 137 116 116 60 70
  Slime     -> ChromaColorRecord 116 94  94  72 82

  Yellow    -> ChromaColorRecord 88  125 125 77 87
  Bronze    -> ChromaColorRecord 66  57  57  68 78
  Mocha     -> ChromaColorRecord 58  30  30  51 61

  Orange    -> ChromaColorRecord 58  101 101 63 73
  DarkRed   -> ChromaColorRecord 49  130 130 31 31
  Red       -> ChromaColorRecord 38  120 120 65 65
  Rose      -> ChromaColorRecord 5   84  84  50 60

  Pink      -> ChromaColorRecord 345 96  96  59 69
  Purple    -> ChromaColorRecord 316 100 100 46 56
  Lavender  -> ChromaColorRecord 307 100 100 64 74





--------------------------------------------------------------------------------
-- Initial Record States

initialHomePageInfo = HomePage
  {streamScreenSize = False -- not <| List.member liveInfo.streamStatus [Nothing, Just Offline]
  ,subPage = MainMainBoxPage -- initialHomeSubPageSupportInfo
  ,history = []
  ,unappendHistory = Nothing
  ,initiateSubPageSliding = False
  ,subPageSlideDirection = True -- True = left to right
  ,initiateSubPageHeaderSliding = False
  ,subPageHeaderSlideDirection = False
  -- NavBar
  ,streamTitleLength = 0
  ,hoverTitle = False
  ,counterPosition = 0
    --SubscribeOverlay {tierSelection = 1
    --                 ,recipient = Neutral
    --                 ,autoRenew = False
    --                 ,giftFriendSearch = ""
    --                 ,numberOfRandomGiftSubs = 5
    --                 ,page2Info = Just {month3Package = False
    --                                   ,message = ""}}
  ,chatBox = initialChatBox}
initialChatPageInfo = ChatPage
  {chatBox = initialChatBox}

initialChatStreamPageInfo = ChatStreamPage
  {messageRoom = initialMessageRoom}

initialStreamerPageInfo = StreamerPage
  {chatBox = initialChatBox
  ,modRoom = initialChatRoom
  ,atMessageRoom = initialMessageRoom
  ,streamStatus = Just True -- Nothing == Offline, Just False == Hosting, Just True == Streaming
  ,streamingTitle = ""
  ,hostingSearch = ""
  ,hostingCheck = 0
  }

initialChatBox =
  {chatBoxOverlay = NoChatBoxOverlay
  ,elmBar = initialElmBar
  --,chatBoxOverlayHistory : Maybe ChatBoxOverlay
  ,initiateChatBoxOverlayHeaderSliding = False
  ,chatBoxSlideDirection = True
  ,chatRoom = initialChatRoom}

initialChatRoom =
  {chatRoomOverlay = NoChatRoomOverlay
  ,mentionBox = Nothing
  ,inputFocused = False
  ,input = ""
  ,messageRoom = initialMessageRoom}

initialMessageRoom =
  {viewport = Nothing
  ,infiniteScroll = Nothing
  ,autoScroll = Just True
  ,content = {highlightedUsers = []
             ,hoverUsername = False}}

initialElmBar =
  {viewport = Nothing
  ,infiniteScroll = Just {direction = False, stack = 1}
  ,autoScroll = Nothing
  ,content = ()}

initialMainMainBoxPageInfo = MainMainBoxPage

initialHomeSubPageSupportInfo = HomeSubPageSupport
  {headerPosition = 1}
initialHomeSubPageDonateInfo = HomeSubPageDonate
  {name = ""
  ,amount = ""
  ,message = ""}
initialHomeSubPageSubscribeInfo = HomeSubPageSubscribe
  {tier = 1
  ,initiateTierSliding = False
  ,tierSlideDirection = True
  ,recipient = Positive
  ,autoRenew = False
  ,giftFriendSearch = ""
  ,inValidGiftFriendSearch = 0
  ,numberOfRandomGiftSubs = 5}

initialHomeSubPageAgreementInfo = HomeSubPageAgreement


--------------------------------------------------------------------------------


isSubPageOn : HomeSubPage -> HomeSubPage -> Bool
isSubPageOn subPage mainBoxPage =
  case (subPage, mainBoxPage) of
    (MainMainBoxPage, MainMainBoxPage) -> True
    (HomeSubPageSupport _, HomeSubPageSupport _) -> True
    (HomeSubPageDonate _, HomeSubPageDonate _) -> True
    (HomeSubPageSubscribe _, HomeSubPageSubscribe _) -> True
    (HomeSubPageAgreement, HomeSubPageAgreement) -> True
    _ -> False


isChatBoxOverlayOn chatBox chatBoxOverlay =
  case (chatBox.chatBoxOverlay, chatBoxOverlay) of
    (ShoutOutBoxOverlay, ShoutOutBoxOverlay) -> True
    (PointsRewardsOverlay, PointsRewardsOverlay) -> True
    (WhispersOverlay _, WhispersOverlay _) -> True
    (SettingsOverlay _, SettingsOverlay _) -> True
    (CommunityOverlay _, CommunityOverlay _) -> True
    (RegisterOverlay _, RegisterOverlay _) -> True
    _ -> False

isChatRoomOverlayOn chatRoom chatRoomOverlay =
  case (chatRoom.chatRoomOverlay, chatRoomOverlay) of
    (EmoteOverlay, EmoteOverlay) -> True
    _ -> False


getCsrfToken cookie =
  let parser = Parser.succeed (identity)
        |. Parser.chompUntil "csrftoken=" -- XSRF-TOKEN
        |. Parser.token "csrftoken="
        |= Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
  in case Parser.run parser cookie of
       Err err -> Err <| Parser.deadEndsToString err
       Ok okToken -> Ok okToken



--encodeJSON = JE.encode 0 << JE.object
jeMaybe f m = case m of
  Just a -> f a
  _ -> JE.null



defaultRequestResponse response = case response of
  Err err -> httpErrorMsg err
  Ok resMsg -> resMsg





-- quick model updates

setProfileBadges firstBadge secondBadge profile =
  {profile | badges =
    let badges = profile.badges
    in {badges | firstBadge = firstBadge
               , secondBadge = secondBadge}}
