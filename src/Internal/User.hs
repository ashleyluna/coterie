{-# LANGUAGE DeriveAnyClass #-}

module Internal.User where

import Control.Lens hiding ((.=))
import Data.HashSet as HashSet
import Data.Text as Text

import Import.NoFoundation


data User = User
  {_userId             :: Int64
  ,_accountInfo        :: AccountInfo
  ,_creatorInfo        :: Maybe Creator
  --,moderation          :: UserModeration ban and mute
  ,_username           :: Text
  ,_pronouns           :: Maybe Text
  ,_role               :: Role
  ,_badges             :: BadgeCollection
  ,_nameColor          :: NameColor
  --,_points             :: Word
  } deriving Show

--fromUserDB :: UserDB -> User

--------------------------------------------------------------------------------
-- Account Info

data AccountInfo = AccountInfo
  {_userCreationTime   :: Word
  ,_userEmail          :: Text
  ,_lastNameChangeTime :: Word
  ,_numMonthsSubbed    :: Word
  ,_season             :: Word
  -- 3rd party connections
  ,_twitchConn         :: Bool
  ,_googleConn         :: Bool
  } deriving Show

--------------------------------------------------------------------------------
-- Roles

type Role = Either Chatter SpecialRole

data Chatter = Chatter
  {_months :: Word
  ,_subscription :: Maybe Subscription
  } deriving Show

data SpecialRole = SpecialRole
  {_roleName :: Text
  ,_power :: Word -- streamer, mods, vips
  } deriving Show

isMod :: Role -> Bool
isMod role = case role of
  Right r -> isMod_ r
  _ -> False
isMod_ :: SpecialRole -> Bool
isMod_ role = case role of
  SpecialRole _ power | power >= 1 -> True
  _ -> False


isAdmin :: Role -> Bool
isAdmin role = case role of
  Right r -> isAdmin_ r
  _ -> False
isAdmin_ :: SpecialRole -> Bool
isAdmin_ role = case role of
  SpecialRole _ power | power == 2 -> True
  _ -> False

--------------------------------------------------------------------------------
-- Badge Collection

data BadgeCollection = BadgeCollection
  {_firstBadge :: Maybe Text
  ,_secondBadge :: Maybe Text
  ,_collection :: HashSet Text -- including the first and second badge
  } deriving Show

setFirstBadge badge badgeCollection =
  if HashSet.member badge $ _collection badgeCollection
     then badgeCollection {_firstBadge = Just badge}
     else badgeCollection

setSecondBadge badge badgeCollection =
  if HashSet.member badge $ _collection badgeCollection
     then badgeCollection {_secondBadge = Just badge}
     else badgeCollection

clearFirstBadge badgeCollection = badgeCollection
  {_firstBadge = _secondBadge badgeCollection
  ,_secondBadge = Nothing}

clearSecondBadge badgeCollection = badgeCollection
  {_secondBadge = Nothing}

instance ToJSON BadgeCollection where
  toJSON (BadgeCollection {..}) = object
    ["first_badge" .= _firstBadge
    ,"second_badge" .= _secondBadge
    ,"collection" .= _collection]

--------------------------------------------------------------------------------
-- Name Color

data NameColor = NameColor
  {_defaultNameColor :: DefaultNameColor
  ,_left :: Maybe ChromaColor
  ,_right :: Maybe ChromaColor
  ,_mode :: ChromaMode
  } deriving (Eq, Show)

type DefaultNameColor = Either DefaultColor DefaultColor -- Left == Random, Right == Chosen

data DefaultColor
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
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data ChromaColor = ChromaColor
  {_hue :: Word
  ,_chromaLight :: Word -- light mode
  ,_chromaDark :: Word -- dark mode
  ,_valueLight :: Word
  ,_valueDark :: Word
  } deriving (Eq, Show)

data ChromaMode
  = RGB
  | LRGB
  | HSI
  | HSL
  | HSV
  | LCH
  | LAB
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)




--defaultNameColor :: DefaultNameColor -> NameColor
--defaultNameColor c = NameColor color Nothing RGB
--  where color = case c of
--          Pink -> ChromaColor 0 80 80 70 90
--          Blue -> ChromaColor 205 80 80 70 90

instance ToJSON ChromaColor where
  toJSON (ChromaColor {..}) = object
    ["hue" .= _hue
    ,"chroma_light" .= _chromaLight
    ,"chroma_dark" .= _chromaDark
    ,"value_light" .= _valueLight
    ,"value_dark" .= _valueDark
    ]

instance FromJSON ChromaColor where
  parseJSON = withObject "ChromaColor" $ \obj -> do
    ChromaColor <$> obj .: "hue"
                <*> obj .: "chroma_light"
                <*> obj .: "chroma_dark"
                <*> obj .: "value_light"
                <*> obj .: "value_dark"



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------







--data ChatMessage
--  = UserMessage UserMessage
--  | SystemMessage String
--  | SubNotification {username :: String
--                    ,tier :: SubTier
--                    ,months :: Int}
--  | GiftSubNotification {gifterUsername :: Maybe String
--                        ,recieverUsername :: String
--                        ,tier :: SubTier}
--  | RawMessage String -- for testing

data UserMessage = UserMessage
  {_senderId :: Int64 -- UserId
  ,_timestamp :: Word
  --,_messageType : Maybe SpecialMessage
  ,_message :: Text
  -- Filter
  --,_curseSwearWords :: Bool
  --,_nsfw :: Bool
  -- Moderation
  ,_removed :: Bool
  }


--type SpecialMessage = HightlightReward




--------------------------------------------------------------------------------


data Creator = Creator
  {_creatorId :: Int64
  } deriving Show



--------------------------------------------------------------------------------





-- combination of DB.ActiveSubscription and DB.Subscription
data Subscription = Subscription
  {_subberId        :: Int64
  ,_paymentId       :: Int64
  ,_subId           :: Int64
  ,_subTier         :: Word
  ,_maybeGifterId   :: Maybe Int64 -- UserId
  ,_is3MonthPackage :: Bool
  ,_subMessage      :: Text
  ,_subSource       :: Text
  ,_recurring       :: Bool
  ,_endTime         :: Word
  } deriving Show






--------------------------------------------------------------------------------
-- Images

data SubBadge = SubBadge
  {monthsRequired :: Word
  ,emote :: Emote
  } deriving (Eq, Show)

instance ToJSON SubBadge where
  toJSON (SubBadge {..}) = object
    ["months_required" .= monthsRequired
    ,"emote" .= emote]

data Emote = Emote
  {name :: Text
  ,image :: Image
  } deriving (Eq, Show, Generic, ToJSON)


data Image = Image
  {width :: Float
  ,height :: Float
  ,url :: Text
  } deriving (Eq, Show, Generic, ToJSON)












--------------------------------------------------------------------------------
-- Make Lenses

makeLenses ''User
makeLenses ''AccountInfo
makeLenses ''Chatter
makeLenses ''SpecialRole
makeLenses ''BadgeCollection
makeLenses ''NameColor
makeLenses ''ChromaColor
makeLenses ''Creator
makeLenses ''Subscription
--makeLenses ''
