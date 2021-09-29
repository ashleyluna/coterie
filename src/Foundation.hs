{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Foundation where

import Control.Concurrent
import Control.Monad.Logger (LogSource)
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Maybe
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist
import Database.Persist.Sql
    ( Entity(Entity),
      PersistStoreRead(get),
      PersistUniqueRead(getBy),
      SqlPersistT,
      SqlBackend,
      runSqlPool,
      ConnectionPool )
import Database.Persist.Types
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import System.Random as SyR
--import qualified Data.Vector as Vec
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.OAuth2.Google
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Web.Cookie

import Import.NoFoundation
import qualified Model as DB
import Streamer

import Prefoundation

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings           :: AppSettings
  , appStatic             :: Static -- ^ Settings for static file serving.
  , appConnPool           :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager        :: Manager
  , appLogger             :: Logger
  ------------------------------------------------------------------------------
  -- Web Socket Connections
  -- write global events
  , tqGlobalEvents        :: TQueue GlobalEvent --GlobalEvent
  -- ws connections read global events with a custom pubsub
  , tvWSConns             :: TVHashMap WSConnId (GESub,TQueue PValue)
  -- for multiple ws connections logged in as the same user
  , tvUserConns           :: TVHash64Map (TVUser, HashMap Int (TQueue Notification))
  ------------------------------------------------------------------------------
  -- Main
  , tvMainChat            :: TVIntMap UserMessage -- timeStamp
  , tvMainChatDelayed     :: TVIntMap UserMessage
  , tvStreamStatus        :: TVar StreamStatus
  --, tvShoutOutBox  :: TVar ShoutOutBox
  --, tvSubGiftersLeaderBoard
  ------------------------------------------------------------------------------
  -- Moderation
  , tvMuteList            :: TVIntMap Int -- userId, timeStamp (end of mute)
  , tvModChat             :: TVIntMap UserMessage -- timeStamp
  --, tvModerationHistory :: TIntMap ModAction -- timeStamp
  ------------------------------------------------------------------------------
  -- Database in memeory
  , tvUsers               :: TVHash64MapTV User

  , tvCreators            :: TVHash64MapTV Creator -- userId

  -- Images --Stand In For Database
  , tvSpecialRoles        :: TVHash64MapTV SpecialRole
  , tvSpecialRoleBadges   :: TVHashMap Text Emote
  , tvSubBadges           :: TVar [IntMap SubBadge]
  , tvSeasonBadges        :: TVar [Emote]
  , tvCommonBadges        :: TVHashMap Text Emote
  , tvGlobalEmoteList     :: TVHashMap Text Emote
  , tvSubOnlyEmoteList    :: TVHashMap Text Emote
  --, pointsRewards    :: UnSet.HashSet PointsReward

  -- Payment
  , tvSubscriptions       :: TVHashMap Int64 Subscription -- userId
  }



-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = sslOnlySessions $ Just <$>
      defaultClientSessionBackend sessionTimeOut "config/client_session_key.aes" -- timeout in minutes

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware
                    . sslOnlyMiddleware sessionTimeOut
                    . defaultCsrfMiddleware
      --let myCsrfSetCookieMiddleware = (>>) $ setCsrfCookieWithCookie
      --      defaultSetCookie {setCookieName = "csrftoken"
      --                       ,setCookiePath = Just "/"}
      --    myCsrfCheckMiddleware handler = csrfCheckMiddleware handler
      --      (getCurrentRoute >>= maybe (return False) isWriteRequest)
      --      "X-CSRFTOKEN"
      --      "_token"
      --in defaultYesodMiddleware
      -- . sslOnlyMiddleware sessionTimeOut
      -- . myCsrfSetCookieMiddleware
      -- . myCsrfCheckMiddleware

    --defaultLayout :: Widget -> Handler Html
    --defaultLayout widget = do

    ---- The page to be redirected to when authentication is required.
    --authRoute
    --    :: App
    --    -> Maybe (Route App)
    --authRoute _ = Just $ AuthR HomeR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized route writable =
      let isAdmin :: Int -> Handler AuthResult
          isAdmin reqPower = maybeAuth >>= \case
            Nothing -> return AuthenticationRequired
            Just (Entity _ DB.User {..}) ->
              userRole ->== runDB . get >>= \case
                Just DB.Role {..} | rolePower >= reqPower -> return Authorized
                _ -> return $ Unauthorized "Your power level isn't high enough"
      in case (route, writable) of
           (AdminPageR, _) -> isAdmin 2
           (StreamerPageR, _) -> isAdmin 2
           (ModPageR, _) -> isAdmin 1
           (ApiR ModR, _) -> isAdmin 1
           _ -> return Authorized


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level = return $
      level == LevelWarn || level == LevelError
        --appShouldLogAll (appSettings app)
        --    || level == LevelWarn
        --    || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomePageR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomePageR)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = DB.UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomePageR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomePageR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ do
      let plugin = case credsPlugin creds of
            "twitch" -> Just TwitchAuth
            "google" -> Just GoogleAuth
            _ -> Nothing
      case plugin of
        Nothing -> return $ ServerError "Invalid plugin"
        Just plugin -> runDB $ do
          let cID = credsIdent creds
          maybeUser <- getByUserAuth plugin cID
          return $ case maybeUser of
            Just (Entity uID _) -> Authenticated uID
            _ -> UserError $ IdentifierNotFound cID
      --let cID = credsIdent creds
      --in case readMay cID of
      --  Just uID -> runDB ¢ get (uID :: DB.UserId) <&> \case
      --    Just _ -> Authenticated uID
      --    _ -> UserError $ IdentifierNotFound cID
      --  _ -> return $ UserError $ IdentifierNotFound cID

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins (App {..}) =
      let Secrets {..} = streamerSecrets
      in [oauth2GoogleScoped ["email"] googleClientId googleClientSecret
         ]

-- TODO do I need this?
-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

data UserAuth = TwitchAuth
              | GoogleAuth

getByUserAuth plugin pluginId = case plugin of
  TwitchAuth -> getBy (DB.UniqueTwitchId pluginId) >>== \(Entity key _) ->
    getBy $ DB.UniqueTwitchAuth $ Just key
  GoogleAuth -> getBy (DB.UniqueGoogleId pluginId) >>== \(Entity key _) ->
    getBy $ DB.UniqueGoogleAuth $ Just key

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
