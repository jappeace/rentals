{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
module Foundation where

import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.Hardcoded
import Yesod.Persist

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import qualified Data.ByteString            as BS
import           Data.Time.Calendar
import           Data.Default               (def)
import           Data.Fixed
import           Data.Kind                  (Type)
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.Sql       (ConnectionPool, runSqlPool)
import           Database.Persist.Sqlite
import           Network.URI
import           Text.Blaze
import           Text.Hamlet
import           Text.Lucius
import           Text.ICalendar
import           Text.Julius
import           Text.Read                  (readMaybe, readEither)

import Settings

data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  }

-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------
data Source = Local | Airbnb | Vrbo
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Source)
-----------------------------------------------------------------------------------------
newtype Money = Money { unMoney :: Centi }
  deriving (Eq, Ord, Num, Fractional)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Money)
-----------------------------------------------------------------------------------------
newtype Slug = Slug { unSlug :: Text }
  deriving (Eq, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Slug)
-----------------------------------------------------------------------------------------
newtype ICS = ICS { unICS :: UUID }
  deriving (Eq, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''ICS)
-----------------------------------------------------------------------------------------

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type DB a = forall (m :: Type -> Type). (MonadUnliftIO m) => ReaderT SqlBackend m a

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-----------------------------------------------------------------------------------------
-- PersistField instances
-----------------------------------------------------------------------------------------
instance PersistField VCalendar where
  toPersistValue = PersistByteString . BS.toStrict . printICalendar def
  fromPersistValue (PersistByteString textCal) =
    case parseICalendar def "." $ BS.fromStrict textCal of
      Right (parsedCal:_, _) -> Right parsedCal
      Left err -> Left . T.pack $ err
instance PersistFieldSql VCalendar where
  sqlType _ = SqlString
-----------------------------------------------------------------------------------------
instance PersistField UUID where
  toPersistValue = PersistText . UUID.toText
  fromPersistValue (PersistText uuid) =
    case UUID.fromText uuid of
      Just uuid' -> Right uuid'
      Nothing -> Left "Failed to create UUID from Text"
instance PersistFieldSql UUID where
  sqlType _ = SqlString
-----------------------------------------------------------------------------------------
instance PersistField Money where
  -- TODO: make double sure that this is precise enough, safe, to deal with money values
  toPersistValue = PersistInt64 . fromIntegral . fromEnum . unMoney
  fromPersistValue (PersistInt64 money) = Right . Money . toEnum . fromIntegral $ money
instance PersistFieldSql Money where
  sqlType _ = SqlInt64
-----------------------------------------------------------------------------------------
instance PersistField URI where
  toPersistValue uri = PersistText . T.pack $ (uriToString id uri) ""
  fromPersistValue (PersistText uri) =
    case parseURI . T.unpack $ uri of
      Just uri' -> Right uri'
      Nothing -> Left "Failed to create URI from Text"
instance PersistFieldSql URI where
  sqlType _ = SqlString
-----------------------------------------------------------------------------------------
instance PersistField Source where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText cs) = left T.pack . readEither $ T.unpack cs
instance PersistFieldSql Source where
  sqlType _ = SqlString
-----------------------------------------------------------------------------------------
instance PersistField Slug where
  toPersistValue = PersistText . unSlug
  fromPersistValue (PersistText slug) = Right $ Slug slug
instance PersistFieldSql Slug where
  sqlType _ = SqlString
-----------------------------------------------------------------------------------------
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . appConnPool

-----------------------------------------------------------------------------------------
-- PathPiece instances
-----------------------------------------------------------------------------------------
instance PathPiece UUID where
  toPathPiece uuid = UUID.toText uuid
  fromPathPiece p = UUID.fromText p
-----------------------------------------------------------------------------------------
instance PathPiece ICS where
  toPathPiece ics = (UUID.toText $ unICS ics) <> ".ics"
  fromPathPiece p = fmap ICS . UUID.fromText . T.reverse . T.drop 4 . T.reverse $ p
-----------------------------------------------------------------------------------------
instance PathPiece Slug where
  toPathPiece = toPathPiece . unSlug
  fromPathPiece = fmap Slug . fromPathPiece

-----------------------------------------------------------------------------------------
-- Content instances
-----------------------------------------------------------------------------------------
instance ToContent VCalendar where
  toContent = toContent . printICalendar def
instance ToTypedContent VCalendar where
  toTypedContent = TypedContent "text/calendar; charset=utf-8" . toContent
-----------------------------------------------------------------------------------------
instance ToContent Listing where
  toContent = toContent . toJSON
instance ToTypedContent Listing where
  toTypedContent = TypedContent "application/json; charset=utf-8" . toContent

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-----------------------------------------------------------------------------------------
-- ToMarkup instances
-----------------------------------------------------------------------------------------
instance ToMarkup Money where
  toMarkup = toMarkup . showFixed False . unMoney
  preEscapedToMarkup = preEscapedToMarkup . showFixed False . unMoney
-----------------------------------------------------------------------------------------
instance ToMarkup Slug where
  toMarkup = toMarkup . unSlug
  preEscapedToMarkup = preEscapedToMarkup . unSlug
-----------------------------------------------------------------------------------------
instance ToMarkup UUID where
  toMarkup = toMarkup . UUID.toText
  preEscapedToMarkup = preEscapedToMarkup . UUID.toText
-----------------------------------------------------------------------------------------
instance ToMarkup URI where
  toMarkup v = toMarkup $ (uriToString id v) ""
  preEscapedToMarkup v = preEscapedToMarkup $ (uriToString id v) ""

-----------------------------------------------------------------------------------------
-- YesodAuth instances
-----------------------------------------------------------------------------------------
instance YesodAuth App where
  type AuthId App = Text

  authPlugins _ = [authHardcoded]

  authLayout = liftHandler . defaultAdminLayout

  loginHandler = authLayout $ do
    isAuthenticated' <- liftHandler maybeAuthId
    when (isJust isAuthenticated') . liftHandler . redirect $ ViewAdminR
    $(whamletFile "templates/admin/login.hamlet")

  redirectToReferer _ = True
  loginDest  _ = ViewAdminR
  logoutDest _ = ViewListingsR

  authenticate Creds {..} = do
    mAdmin <- liftHandler $ lookupAdmin credsIdent
    pure $ case credsPlugin of
      "hardcoded" -> case mAdmin of
        Nothing -> UserError InvalidLogin
        Just m  -> Authenticated $ adminUsername m

instance YesodAuthPersist App where
  type AuthEntity App = Text

  getAuthEntity au = do
    mAdmin <- liftHandler $ lookupAdmin au
    pure . fmap adminUsername $ mAdmin

instance YesodAuthHardcoded App where
  validatePassword u p = liftHandler $ do
    master <- getYesod
    pure $ case find
      (\m -> adminUsername m == u && adminPassword m == p)
      (appAdmin $ appSettings master) of
        Just _ -> True
        _      -> False
  doesUserNameExist u = isJust <$> (liftHandler $ lookupAdmin u)

lookupAdmin :: Text -> Handler (Maybe AppAdmin)
lookupAdmin u = do
  master <- getYesod
  pure $ find (\m -> adminUsername m == u)
    (appAdmin $ appSettings master)

isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must authenticate to access this page"
    Just _ -> Authorized

isAuthenticatedView :: Handler AuthResult
isAuthenticatedView = do
  authResult <- isAuthenticated
  case authResult of
    Unauthorized _ -> redirect $ AuthR LoginR
    Authorized -> pure Authorized

-----------------------------------------------------------------------------------------
instance Yesod App where
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    (30 * 24 * 60) "config/client_session_key.aes"

  isAuthorized ViewAdminR                          _ = isAuthenticatedView
  isAuthorized (ViewAdminListingR _ _)             _ = isAuthenticatedView
  isAuthorized AdminListingSourcesR                _ = isAuthenticated
  isAuthorized AdminListingNewR                    _ = isAuthenticated
  isAuthorized (AdminListingR _)                   _ = isAuthenticated
  isAuthorized (AdminListingImageR _)              _ = isAuthenticated
  isAuthorized (AdminListingUpdateBlockedDatesR _) _ = isAuthenticated
  isAuthorized (AdminListingImportR _)             _ = isAuthenticated
  isAuthorized (AdminListingUpdateDayPriceR _)     _ = isAuthenticated
  isAuthorized (CalendarExportR _)                 _ = pure Authorized

  isAuthorized ViewListingsR                       _ = pure Authorized
  isAuthorized (ViewListingR _ _)                  _ = pure Authorized
  isAuthorized (ListingQuoteR _)                   _ = pure Authorized
  isAuthorized (ListingBookR _)                    _ = pure Authorized
  isAuthorized (ListingBookPaymentSuccessR _)      _ = pure Authorized
  isAuthorized (ListingBookPaymentCancelR _)       _ = pure Authorized

  isAuthorized (ImageR _)                          _ = pure Authorized

  isAuthorized (AuthR _)                           _ = pure Authorized

  defaultLayout contents = do
    pc <- widgetToPageContent contents
    ma <- maybeAuthId
    messages <- getMessage
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

defaultUserLayout :: WidgetFor App () -> Handler Html
defaultUserLayout w = defaultLayout $ do
  toWidgetHead $(juliusFile "templates/script/user/forms.julius")
  toWidgetHead $(luciusFile "templates/style/user.lucius")
  w

defaultAdminLayout :: WidgetFor App () -> Handler Html
defaultAdminLayout w = defaultLayout $ do
  toWidgetHead $(juliusFile "templates/script/admin/forms.julius")
  toWidgetHead $(luciusFile "templates/style/admin.lucius")
  w

