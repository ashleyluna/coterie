{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Global where

import Control.Concurrent
import Control.Lens hiding ((.=))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as P

import Import
import qualified Model as DB

import Postfoundation
import Prefoundation
import Internal.TestSuite

global :: App -> IO ()
global foundation = do
  --setUpDatabaseStandIns foundation
  temporaryDatabaseStandIns foundation
  -- Processes
  globalEvents foundation
  streamStatusEventTimer foundation
  streamStatistics foundation
  subscriptionRemovalTimer foundation
  testSuite foundation
  --streamStatusProcess streamerInfo
  --                    tchanStreamStatus
  --                    tvarStreamStatus


setUpDatabaseStandIns :: App -> IO ()
setUpDatabaseStandIns app@App{..} = do
  let --fullTable :: ReaderT backend IO [Entity a]
      --fullTable = P.selectList [] []
      fromTable :: TVar a -> ReaderT SqlBackend IO dbA -> (dbA -> IO a) -> IO ()
      fromTable tvar tableSelect f = do
        table <- runDB_ app tableSelect
        x <- f table
        atomically $ writeTVar tvar x
      fromImage :: P.Key DB.Image -> IO (Maybe Image)
      fromImage imageId = runDB_ app $ P.get imageId <&&> \DB.Image{..} ->
        Image imageWidth imageHeight imageUrl
      fromEmote :: DB.Emote -> IO (Maybe Emote)
      fromEmote DB.Emote{..} = Emote emoteName <$$> fromImage emoteImage
      hashmapEmoteTable :: (a -> Key DB.Emote) -> [Entity a] -> IO (HashMap Text Emote)
      hashmapEmoteTable f as = HashMap.fromList . catMaybes <$> as `for` \(Entity _ a) -> do
        emote <- runDB_ app (P.get $ f a) >>== fromEmote
        return $ emote <&> \emote@Emote{..} -> pair name emote

  -- special roles
  fromTable tvSpecialRoles (P.selectList [] []) $ 
    \specialRoles -> HashMap.fromList <$> specialRoles `for` \(Entity key DB.Role{..}) -> do
      tvRole <- newTVarIO $ SpecialRole (P.fromSqlKey key) roleName rolePower roleOrder
      return $ pair (P.fromSqlKey key) tvRole
  -- sub badges
  fromTable tvSubBadges (P.selectList [] [P.Asc DB.SubBadgeMonthsRequired]) $
    \subBadges -> 
          map (IntMap.fromList . map snd)
        . groupBy (\(tier1,_) (tier2, _) -> tier1 == tier2)
        . catMaybes
      <$> subBadges `for` \(Entity _ DB.SubBadge{..}) -> do
            emote <- runDB_ app (P.get subBadgeEmote) >>== fromEmote
            return $ pair subBadgeTier
                   . pair subBadgeMonthsRequired
                   . SubBadge subBadgeMonthsRequired
                 <$> emote
  -- season badges
  fromTable tvSeasonBadges (P.selectList [] [P.Asc DB.SeasonBadgeNumber]) $
    \seasonBadges -> catMaybes <$> seasonBadges `for` \(Entity _ DB.SeasonBadge{..}) ->
      runDB_ app (P.get seasonBadgeEmote) >>== fromEmote 
  -- special role badges
  fromTable tvSpecialRoleBadges (P.selectList [] []) $
    hashmapEmoteTable DB.roleBadgeEmote
  -- common badges
  fromTable tvCommonBadges (P.selectList [] []) $
    hashmapEmoteTable DB.commonBadgeEmote
  -- global emotes
  fromTable tvGlobalEmoteList (P.selectList [] []) $
    hashmapEmoteTable DB.globalEmoteEmote
  -- sub only emotes
  fromTable tvSubOnlyEmoteList (P.selectList [] []) $
    hashmapEmoteTable DB.subEmoteEmote
  
  -- subscriptions
  --fromTable tvSubscriptions $



  -- users
  fromTable tvUsers (P.selectList [] []) $
    \users -> do
      users <- users `for` fromUserDB app
      HashMap.fromList <$> users `for` \user@User{..} -> do
        tvUser <- newTVarIO user
        return $ pair _userId tvUser




globalEvents :: App -> IO ()
globalEvents App{..} = void $ forkIO $ forever $ atomically $ do
  globalEvent <- readTQueue tqGlobalEvents
  HashMap.elems <$> readTVar tvWSConns >>=- \(pubsub, tqueue) -> do
    toGlobalEventJSON <- toPJSON globalEvent
    let push = writeTQueue tqueue toGlobalEventJSON
    case globalEvent of
      MainChatGlobalEvent _     | receiveMainChat     pubsub -> push
      StreamStatusGlobalEvent _ | receiveStreamStatus pubsub -> push
      ModGlobalEvent _          | recieveModEvents    pubsub -> push
      _ -> return ()




{-
every 4 minutes, update the Streamstatus viewerCount
using the number of user connections from tvUserConns
and send all all
-}
streamStatusEventTimer :: App -> IO ()
streamStatusEventTimer App{..} = void $ forkIO $ forever $ do
  threadDelay $ 4 * minutes
  atomically $ do
    numOfUsers <- HashMap.size <$> readTVar tvUserConns
    streamStatus <- set viewerCount numOfUsers <$> readTVar tvStreamStatus
    writeTVar tvStreamStatus streamStatus
    writeTQueue tqGlobalEvents $ StreamStatusGlobalEvent streamStatus

streamStatistics :: App -> IO ()
streamStatistics App{..} = void $ forkIO $ forever $ do
  threadDelay $ 10 * minutes
  {-
  things to keep track of ... in a 10 minute window
    - number of connected users (not including ws connections per user)
    - number of active users (based on messsges sent, distungish between active subscribers)
    - number of messages sent
    - numnber of accounts created
    - number of subscriptions
  -}






subscriptionRemovalTimer :: App -> IO ()
subscriptionRemovalTimer App{..} = do
  return ()




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




fromUserDB :: App -> Entity DB.User -> IO User
fromUserDB app@App{..} userEntity@(Entity userKey user@DB.User{..}) = do
  let userId = E.fromSqlKey userKey
      badges = BadgeCollection userFirstBadge userSecondBadge $
        HashSet.fromList $ words userBadgeCollection
      maybeLeft = ChromaColor <$> userColorLeftH
                              <*> userColorLeftCL
                              <*> userColorLeftCD
                              <*> userColorLeftVL
                              <*> userColorLeftVD
      maybeRight = ChromaColor <$> userColorRightH
                               <*> userColorRightCL
                               <*> userColorRightCD
                               <*> userColorRightVL
                               <*> userColorRightVD
  accountInfo <- do
    twitchConn <- userTwitchAuth ->== runDB_ app . get >>=- \DB.TwitchAuth{..} -> 
      return $ TwitchConn twitchAuthUserId twitchAuthAccessToken twitchAuthRefreshToken
                          twitchAuthLogin twitchAuthEmail twitchAuthCreatedTime
                          twitchAuthFollowTime twitchAuthIsSubscriber
    return $ AccountInfo userCreationTime userEmail userLastNameChangeTime
                         userNumMonthsSubbed userSeason
                         twitchConn
    --() -- google conn
  defaultNameColor <- case userDefaultColor >>= readMay of
    Just color -> return $ Right color
    _ -> Left <$> randomDefaultColor userCreationTime
  specialRoles <- liftIO $ readTVarIO tvSpecialRoles
  role <- case userRole >>= flip HashMap.lookup specialRoles . E.fromSqlKey of
    Just specialRole -> return $ Right specialRole
    _ -> liftIO $ (Left . Chatter userNumMonthsSubbed) .
                  -- check if user has a subscription
                  HashMap.lookup userId <$> readTVarIO tvSubscriptions
  let nameColor = NameColor defaultNameColor
                            maybeLeft
                            maybeRight
                          $ fromMaybe LRGB $ readMay userColorMode
  creatorInfo <- HashMap.lookup userId <$> readTVarIO tvCreators
  moderation <- UserModeration <$> return userMeaningfulMessages
                               <*> return []
                               <*> return 0
                               <*> return False
  return $ User userId
                accountInfo
                creatorInfo
                moderation
                userUsername
                userPronouns
                role
                badges
                nameColor
                -- points




runDB_ :: App -> SqlPersistT IO a -> IO a
runDB_ master action = P.runSqlPool action $ appConnPool master















--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------




temporaryDatabaseStandIns :: App -> IO ()
temporaryDatabaseStandIns App{..} = atomically $ do
  -- Main
  --tqGlobalEvents
  --tvWSConns
  --tvUserConns


  -- Main
  --tvMainChat
  --tvMainChatDelayed
  writeTVar tvStreamStatus $
    Streaming (TwitchStream "theserfstv")
              "Abby Martin Interview // Israel Attacks Post Ceasefire // Toronto Police Thin Blue Line // Dennis Prager Debunks John Oliver // Hot Tub META"
              --"Christian Action Movie Review"
              1618617316
              780
  -- shoutOutBox

  -- Moderation
  --tvMuteList
  --tvModChat

  --tvUsers

  --tvCreators

  -- Images
  writeTVar tvSpecialRoles =<< specialRoles defaultStaticInfo
  writeTVar tvSpecialRoleBadges $ specialRoleBadges defaultStaticInfo
  writeTVar tvSubBadges $ subBadges defaultStaticInfo
  writeTVar tvSeasonBadges $ seasonBadges defaultStaticInfo
  writeTVar tvCommonBadges $ commonBadges defaultStaticInfo
  writeTVar tvGlobalEmoteList $ globalEmoteList defaultStaticInfo
  writeTVar tvSubOnlyEmoteList $ subOnlyEmoteList defaultStaticInfo

  -- Payment
  writeTVar tvSubscriptions $ subscriptions defaultStaticInfo




-- for testing only
data DefaultStaticInfo = DefaultStaticInfo
  {specialRoles      :: STM (HashMap Int64 (TVar SpecialRole))
  ,specialRoleBadges :: HashMap Text Emote
  ,subBadges         :: [IntMap SubBadge]
  ,seasonBadges      :: [Emote]
  ,commonBadges      :: HashMap Text Emote
  ,globalEmoteList   :: HashMap Text Emote
  ,subOnlyEmoteList  :: HashMap Text Emote
  ,subscriptions     :: HashMap Int64 Subscription
  }

defaultStaticInfo :: DefaultStaticInfo
defaultStaticInfo = DefaultStaticInfo
  {specialRoles = HashMap.fromList <$> sequenceA 
     [pair 0 <$> newTVar (SpecialRole 0 "Vanguard" 2 1)
     ,pair 1 <$> newTVar (SpecialRole 1 "Vanguard" 1 2)
     ,pair 2 <$> newTVar (SpecialRole 2 "Creator"  0 3)
     ]
  ,specialRoleBadges = HashMap.fromList $ fmap (\emote -> (name emote, emote))
     [Emote "Vanguard" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/303024760/default/light/1.0"
     ,Emote "Creator" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/303100435/default/light/1.0"
     ]
  ,subBadges = IntMap.fromList . fmap (\subBadge -> (monthsRequired subBadge,subBadge)) <$>
     [[SubBadge 1 $ Emote "Happy Bread"   $ Image 18 18 "https://static-cdn.jtvnw.net/badges/v1/84f7a89b-6d08-49df-b0b4-c4f9df6e815a/1"
      ,SubBadge 2 $ Emote "Dancing Bread" $ Image 18 18 "https://static-cdn.jtvnw.net/badges/v1/bcb8f7cd-1dc3-43ae-85a1-7d2c02cf8d38/1"
      ,SubBadge 3 $ Emote "Gnarly Bread"  $ Image 18 18 "https://static-cdn.jtvnw.net/badges/v1/e0508a49-9389-443c-abb8-82b6769aa2f3/1"
      ]
     ,[SubBadge 1 $ Emote "Lance Hype" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/304156582/default/light/1.0"
      ,SubBadge 3 $ Emote "Lance Ceo" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/304156571/default/light/1.0"
      ,SubBadge 6 $ Emote "Lance Among Us" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/emotesv2_e709157a391141e1b2468b6e8543174a/default/light/1.0"
      ]
     ,[SubBadge 1 $ Emote "Dave Cozy" $ Image 18 18 "https://static-cdn.jtvnw.net/badges/v1/410b5cc0-b189-46a0-9afe-9c6691e0bff9/1"
      ,SubBadge 3 $ Emote "Dave Pog" $ Image 18 18 "https://static-cdn.jtvnw.net/badges/v1/479f57b0-60c4-407d-ba97-03ebacb6cce6/1"
      ]
     ]
  ,seasonBadges =
     [Emote "Butter Horse" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/303801426/default/light/1.0"
     ,Emote "Spineless" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/302260978/default/light/1.0"
     --,Emote "Candy" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/301760130/default/light/1.0"
     ]
  ,commonBadges = HashMap.fromList
     [pair "Beta Tester" $ Emote "Mondragon" $ Image 28 28 "https://static-cdn.jtvnw.net/emoticons/v2/307503977/default/light/1.0"
     ]
  ,globalEmoteList = HashMap.fromList $ fmap (\emote -> (name emote, emote))
     [Emote "serfshype" $ Image 698 698 "https://cdn.discordapp.com/attachments/645827500609372170/715384641879670904/twitchemoteExport0001.png"
     ,Emote "serfspog" $ Image 698 698 "https://cdn.discordapp.com/attachments/645827500609372170/715384656396156928/twitchemoteExport0007.png"
     ]
  ,subOnlyEmoteList = HashMap.fromList $ fmap (\emote -> (name emote, emote))
     [Emote "serfscry" $ Image 698 698 "https://cdn.discordapp.com/attachments/645827500609372170/715384660032618556/twitchemoteExport0010.png"
     ,Emote "serfshmm" $ Image 1000 1000 "https://cdn.discordapp.com/attachments/645827500609372170/715384648590426203/twitchemoteExport0003.png"
     ]
  ,subscriptions = HashMap.fromList 
    [--pair 2 $ Subscription
    ]
  }