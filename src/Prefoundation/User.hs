{-# LANGUAGE DeriveAnyClass #-}

module Prefoundation.User where

import Control.Lens hiding ((.=))
import Data.Char as Char
import Data.HashSet as HashSet
import Data.Text as Text

import Import.NoFoundation


data User = User
  {_userId             :: UserId
  ,_accountInfo        :: AccountInfo
  ,_creatorInfo        :: Maybe TVCreator
  ,_moderation         :: UserModeration
  ,_username           :: Text
  ,_pronouns           :: Maybe Text
  ,_role               :: Role
  ,_badges             :: BadgeCollection
  ,_nameColor          :: NameColor
  --,_points             :: Int
  }

type UserId = Int64
type TVUser = TVar User

--------------------------------------------------------------------------------
-- Account Info

data AccountInfo = AccountInfo
  {_userCreationTime   :: Int
  ,_userEmail          :: Text
  ,_lastNameChangeTime :: Int
  ,_numMonthsSubbed    :: Int
  ,_season             :: Int
  -- 3rd party connections
  ,_twitchConn         :: Maybe TwitchConn
  --,_googleConn         :: ()
  } deriving Show

data TwitchConn = TwitchConn
  {twitchId           :: Text
  ,twitchAccessToken  :: Text
  ,twitchRefreshToken :: Text
  ,twitchLogin        :: Text
  ,twitchEmail        :: Text
  ,twtichCreatedTime  :: Int
  ,twitchFollowTime   :: Maybe Int
  ,twitchIsSubscriber :: Maybe Int
  } deriving Show

-- User Moderation

data UserModeration = UserModeration
  {_meaningfulMessages :: Int -- number of messages (not only emotes) sent during stream
  ,_last5MessagesTimes :: [Int]
  ,_numModActions :: Int
  ,_isOnWatchList :: Bool
  } deriving Show

isSafeUser :: UserModeration -> Bool
isSafeUser moderation = _meaningfulMessages moderation >= 1000

isPronounsCorrectFormat :: Text -> Bool
isPronounsCorrectFormat newPronouns = not
   $ betweenLen 2 10 newPronouns
  && Text.all (\c -> Char.isAlphaNum c || c == '/') newPronouns

--------------------------------------------------------------------------------
-- Roles

{-
Role uses Either to allow Chatter and SpecialRole to be distinguished types.
If SpecialRole were the same type as Chatter, a Chatter value could accidently
be added into tvSpecialRoles which is specifically a collction of SpecialRoles.
-}

type Role = Either Chatter TVSpecialRole

data Chatter = Chatter
  {_months :: Int
  ,_subscription :: Maybe (TVar Subscription)
  }

data SpecialRole = SpecialRole
  {_roleId   :: Int64
  ,_roleName :: Text
  ,_power    :: Int -- streamer, mods, vips
  ,_order    :: Int
  } deriving Show
type TVSpecialRole = TVar SpecialRole

getRolePower :: Role -> STM Int
getRolePower role = case role of
  Right r -> readTVar r <&> _power
  _ -> return 0

isMod :: Role -> STM Bool
isMod role = case role of
  Right r -> readTVar r <&> isMod_ 
  _ -> return False
isMod_ :: SpecialRole -> Bool
isMod_ role = case role of
  SpecialRole{..} | _power >= 1 -> True
  _ -> False

isAdmin :: Role -> STM Bool
isAdmin role = case role of
  Right r -> readTVar r <&> isAdmin_
  _ -> return False
isAdmin_ :: SpecialRole -> Bool
isAdmin_ role = case role of
  SpecialRole{..} | _power == 2 -> True
  _ -> False


--------------------------------------------------------------------------------
-- Badge Collection

data BadgeCollection = BadgeCollection
  {_firstBadge :: Maybe Text
  ,_secondBadge :: Maybe Text
  ,_collection :: HashSet Text -- including the first and second badge
  } deriving Show

setFirstBadge :: Text -> BadgeCollection -> BadgeCollection
setFirstBadge badge badgeCollection =
  if HashSet.member badge $ _collection badgeCollection
     then badgeCollection {_firstBadge = Just badge}
     else badgeCollection

setSecondBadge :: Text -> BadgeCollection -> BadgeCollection
setSecondBadge badge badgeCollection =
  if HashSet.member badge $ _collection badgeCollection
     then badgeCollection {_secondBadge = Just badge}
     else badgeCollection

clearFirstBadge :: BadgeCollection -> BadgeCollection
clearFirstBadge badgeCollection = badgeCollection
  {_firstBadge = _secondBadge badgeCollection
  ,_secondBadge = Nothing}

clearSecondBadge :: BadgeCollection -> BadgeCollection
clearSecondBadge badgeCollection = badgeCollection
  {_secondBadge = Nothing}

instance ToJSON BadgeCollection where
  toJSON BadgeCollection{..} = object
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
  {_hue :: Int
  ,_chromaLight :: Int -- light mode
  ,_chromaDark :: Int -- dark mode
  ,_valueLight :: Int
  ,_valueDark :: Int
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
  toJSON ChromaColor {..} = object
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
  {_sender :: TVUser -- UserId
  ,_timestamp :: Int
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
  ,_creatorName :: Bool
  } deriving Show
type TVCreator = TVar Creator


--------------------------------------------------------------------------------





-- combination of DB.ActiveSubscription and DB.Subscription
data Subscription = Subscription
  {_subscriptionId  :: Int64
  ,_paymentId       :: Int64
  ,_subber          :: TVUser
  ,_maybeGifter     :: Maybe TVUser
  ,_tier            :: Int
  ,_is3MonthPackage :: Bool
  ,_subMessage      :: Text
  ,_subSource       :: Text
  ,_recurring       :: Bool
  ,_endTime         :: Int
  }




--------------------------------------------------------------------------------
-- Images

data SubBadge = SubBadge
  {monthsRequired :: Int
  ,emote :: Emote
  } deriving (Eq, Show)

instance ToJSON SubBadge where
  toJSON SubBadge {..} = object
    ["months_required" .= monthsRequired
    ,"emote" .= emote]

data Emote = Emote
  {name :: Text
  ,image :: Image
  } deriving (Eq, Show, Generic, ToJSON)


data Image = Image
  {width :: Double
  ,height :: Double
  ,url :: Text
  } deriving (Eq, Show, Generic, ToJSON)












--------------------------------------------------------------------------------
-- Make Lenses

makeLenses ''User
makeLenses ''AccountInfo
makeLenses ''UserModeration
makeLenses ''Chatter
makeLenses ''SpecialRole
makeLenses ''BadgeCollection
makeLenses ''NameColor
makeLenses ''ChromaColor
makeLenses ''Creator
makeLenses ''Subscription
--makeLenses ''
