{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes #-}
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


module Rentals.Foundation where

import Yesod
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.Hardcoded
import Yesod.Persist

import Rentals.Database.Listing
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

import Rentals.Settings

data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  }

newtype ICS = ICS { unICS :: UUID }
  deriving (Eq, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''ICS)
-----------------------------------------------------------------------------------------

mkYesodData "App" [parseRoutes|
/admin                                         ViewAdminR                      GET
/admin/listing/view/#ListingId/#Slug           ViewAdminListingR               GET

/admin/listing/sources                         AdminListingSourcesR            GET
/admin/listing/new                             AdminListingNewR                         PUT
/admin/listing/list/#ListingId                 AdminListingR                   GET      PUT
/admin/listing/image/#ListingId                AdminListingImageR                       PUT DELETE
/admin/listing/update-blocked-dates/#ListingId AdminListingUpdateBlockedDatesR          PUT
/admin/listing/import/#ListingId               AdminListingImportR                      PUT DELETE
/admin/listing/update-day-price/#ListingId     AdminListingUpdateDayPriceR              PUT

/ical/export/#ICS                              CalendarExportR                 GET

/                                              ViewListingsR                   GET
/view/listing/#ListingId/#Slug                 ViewListingR                    GET
/view/book/success                             ViewBookSuccessR                GET

/listing/quote/#ListingId                      ListingQuoteR                       POST
/listing/book/#ListingId                       ListingBookR                             PUT
/listing/book/#ListingId/payment/success       ListingBookPaymentSuccessR      GET
/listing/book/#ListingId/payment/cancel        ListingBookPaymentCancelR       GET

/image/#UUID                                   ImageR                          GET
/auth                                          AuthR                           Auth getAuth

|]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

type DB a = forall (m :: Type -> Type). (MonadUnliftIO m) => ReaderT SqlBackend m a

-----------------------------------------------------------------------------------------
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . appConnPool

-----------------------------------------------------------------------------------------
-- PathPiece instances
-----------------------------------------------------------------------------------------
instance PathPiece ICS where
  toPathPiece ics = (UUID.toText $ unICS ics) <> ".ics"
  fromPathPiece p = fmap ICS . UUID.fromText . T.reverse . T.drop 4 . T.reverse $ p


-----------------------------------------------------------------------------------------
-- Content instances
-----------------------------------------------------------------------------------------
instance ToContent VCalendar where
  toContent = toContent . printICalendar def
instance ToTypedContent VCalendar where
  toTypedContent = TypedContent "text/calendar; charset=utf-8" . toContent
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- ToMarkup instances
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
instance ToMarkup UUID where
  toMarkup = toMarkup . UUID.toText
  preEscapedToMarkup = preEscapedToMarkup . UUID.toText
-----------------------------------------------------------------------------------------

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
  isAuthorized ViewBookSuccessR                    _ = pure Authorized
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

