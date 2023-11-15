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

import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString            as BS
import           Data.Default               (def)
import           Data.Fixed
import           Data.Kind                  (Type)
import           Data.List                  (find)
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
import           Text.Blaze
import           Text.Hamlet
import           Text.ICalendar
import           Text.Read                  (readMaybe)

import Settings

data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
  }

newtype Money = Money { unMoney :: Centi }

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
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . appConnPool

-----------------------------------------------------------------------------------------
-- PathPiece instances
-----------------------------------------------------------------------------------------
instance PathPiece UUID where
  toPathPiece uuid = (UUID.toText uuid) <> ".ics"
  fromPathPiece p = UUID.fromText . T.drop 4 $ p
-----------------------------------------------------------------------------------------
instance PathPiece (Either Text UserId) where
  toPathPiece = T.pack . show
  fromPathPiece = readMaybe . T.unpack

-----------------------------------------------------------------------------------------
-- Content instances
-----------------------------------------------------------------------------------------
instance ToContent VCalendar where
  toContent = toContent . printICalendar def
instance ToTypedContent VCalendar where
  toTypedContent = TypedContent "text/calendar; charset=utf-8" . toContent

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-----------------------------------------------------------------------------------------
-- ToMarkup instances
-----------------------------------------------------------------------------------------
instance ToMarkup Money where
  toMarkup = toMarkup . showFixed False . unMoney
  preEscapedToMarkup = preEscapedToMarkup . showFixed False . unMoney

-----------------------------------------------------------------------------------------
-- YesodAuth instances
-----------------------------------------------------------------------------------------
instance YesodAuth App where
  type AuthId App = Either Text UserId

  authPlugins _ = [authHardcoded]

  authenticate Creds {..} = do
    mAdmin <- liftHandler $ lookupAdmin credsIdent
    pure $ case credsPlugin of
      "hardcoded" -> case mAdmin of
        Nothing -> UserError InvalidLogin
        Just m  -> Authenticated . Left $ adminUsername m

instance YesodAuthPersist App where
  type AuthEntity App = Either AppAdmin User

  getAuthEntity (Right uid) = do
    x <- liftHandler . runDB $ get uid
    pure . fmap Right $ x
  getAuthEntity (Left au) = do
    mAdmin <- liftHandler $ lookupAdmin au
    pure (Left <$> mAdmin)

instance YesodAuthHardcoded App where
  validatePassword u p = do
    isValid <- liftHandler $ lookupAdminPassword u p
    pure isValid
  doesUserNameExist u = do
    mAdmin <- liftHandler $ lookupAdmin u
    pure $ isJust mAdmin

lookupAdminPassword :: Text -> Text -> Handler Bool
lookupAdminPassword u p = do
  master <- getYesod
  pure $ case find
    (\m -> adminUsername m == u && adminPassword m == p)
    (appAdmin $ appSettings master) of
      Just _ -> True
      _      -> False

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

-----------------------------------------------------------------------------------------
instance Yesod App where
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    (30 * 24 * 60) "config/client_session_key.aes"

  isAuthorized HomeR _ = pure Authorized

  defaultLayout contents = do
    pc <- widgetToPageContent contents
    messages <- getMessage
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")
