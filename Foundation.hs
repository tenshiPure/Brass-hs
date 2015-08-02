module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Data.Time

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
--         master <- getYesod
--         mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
--             addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
--     isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = PersonId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- insertBy $ Person (credsIdent creds) (credsIdent creds) "default.png"
        return $ Just $
            case x of
                Left (Entity personId _) -> personId
                Right personId -> personId

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


-- page type
-- todo これからタイトル作るのが良いかも
data Page = PHome | PMessage | PSchedule | PLink deriving (Show, Eq)


-- require login, title and current group id.
renderWithGroups :: Widget -> Html -> Page -> GroupId -> [Widget] -> Handler Html
renderWithGroups mainWidget title page currentGroupId widgets = do
    authId <- requireAuthId

    mBelong <- runDB $ selectFirst [BelongGroupId ==. currentGroupId, BelongPersonId ==. authId] [Asc BelongId]

    case mBelong of
        (Just _) -> do
            loginPerson <- runDB $ get404 authId

            belongs <- runDB $ selectList [BelongPersonId ==. authId] [Asc BelongId]
            let belongGroupIds = map (belongGroupId . entityVal) belongs
            groups <- runDB $ selectList [GroupId <-. belongGroupIds] [Asc GroupId]

            (groupCreateWidget, groupCreateEnctype) <- generateFormPost $ fGroup

            defaultLayout $ do
                mapM_ toWidget widgets
                toWidget ($(widgetFile "widget/common") :: Widget)

                let headerWidget      = $(widgetFile "layout/header")
                let groupWidget       = $(widgetFile "layout/group")
                let tabWidget         = $(widgetFile "layout/tab")
                let groupManageWidget = $(widgetFile "layout/group-manage")

                let modalGroupCreateWidget = $(widgetFile "modal/group-create")
                let modalInviteLinkWidget = $(widgetFile "modal/invite-link")

                setTitle title
                $(widgetFile "layout/frame")

        Nothing  -> do
            redirect $ HomeR


-- 07/14 20:12:05
format :: TimeZone -> UTCTime -> String
format timezone utctime = formatTime defaultTimeLocale "%m/%d %H:%M:%S" zonedTime
    where
        zonedTime = utcToZonedTime timezone utctime


-- 07/14 20:12:05 (火)
formatWithWeek :: TimeZone -> UTCTime -> String
formatWithWeek timezone utctime = formated ++ " (" ++ week ++ ")"
    where
        formated = format timezone utctime
        zonedTime = utcToZonedTime timezone utctime
        week = case (formatTime defaultTimeLocale "%w" zonedTime) of
            "0" -> "日"
            "1" -> "月"
            "2" -> "火"
            "3" -> "水"
            "4" -> "木"
            "5" -> "金"
            "6" -> "土"
            _   -> ""

-- create with id ant attrs
createSettings :: Text -> [(Text, Text)] -> FieldSettings master
createSettings id' attrs = FieldSettings {
                               fsLabel = "",
                               fsId = Just id',
                               fsName = Nothing,
                               fsAttrs = attrs,
                               fsTooltip = Nothing
                           }


fGroup :: Html -> MForm Handler (FormResult Group, Widget)
fGroup extra = do
    (nameResult, nameView) <- mreq textField "" Nothing
    (iconResult, iconView) <- mreq textField "" Nothing
    let result = Group
           <$> nameResult
           <*> iconResult
        widget = $(widgetFile "group/form/group")
    return (result, widget)


toIntId :: ToBackendKey SqlBackend record => Key record -> Int64
toIntId = fromSqlKey


createBelong :: (YesodAuth master, YesodPersist master, AuthId master ~ Key Person, YesodPersistBackend master ~ SqlBackend) => GroupId -> PersonId -> HandlerT master IO ()
createBelong groupId personId = do
    now <- liftIO getCurrentTime
    _ <- runDB $ insert $ Belong groupId personId
    _ <- runDB $ insert $ BelongLog 0 now groupId personId

    return ()


deleteBelong :: (YesodAuth master, YesodPersist master, AuthId master ~ Key Person, YesodPersistBackend master ~ SqlBackend) => GroupId -> PersonId -> HandlerT master IO ()
deleteBelong groupId personId = do
    now <- liftIO getCurrentTime
    _ <- runDB $ deleteWhere [BelongGroupId ==. groupId, BelongPersonId ==. personId]
    _ <- runDB $ insert $ BelongLog 1 now groupId personId

    return ()
