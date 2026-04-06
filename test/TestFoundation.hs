{-# LANGUAGE OverloadedStrings #-}

-- | Test application setup using SQLite in-memory database
module TestFoundation
  ( makeTestApp
  , testDB
  ) where

import Rentals.Foundation (App(..))
import Rentals.Settings
import Rentals.Database (migrateAll)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.Sql (runSqlPool, runMigration, SqlPersistT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Network.Mail.Pool (SmtpCred(..))
import Yesod.Test (YesodExample, getTestYesod)
import System.Directory (createDirectoryIfMissing, removeFile)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import qualified Data.Text as T

-- | Dummy settings for testing — Stripe and SMTP are never actually called
testSettings :: AppSettings
testSettings = AppSettings
  { appPort = 3000
  , appEmail = "test@example.com"
  , appAdmin = [AppAdmin
    { adminUsername = "admin"
    , adminPassword = "admin"
    , adminEmail = "admin@example.com"
    }]
  , appDatabase = AppDatabase
    { user = ""
    , password = ""
    , host = ""
    , port = 0
    , database = ""
    , poolsize = 1
    , migrationFolder = MigrationFolder "migrations/up"
    }
  , appSmtpCreds = SmtpCred "localhost" "" "" 2525
  , appStripe = Stripe
    { stripeSecret = "sk_test_dummy"
    , stripePublic = "pk_test_dummy"
    }
  , appEnv = EnvDev
  }

testDbPath :: String
testDbPath = "/tmp/rentals-test.sqlite3"

-- | Create a test App with SQLite file-based database
makeTestApp :: IO App
makeTestApp = do
  createDirectoryIfMissing True "config"
  _ <- try (removeFile testDbPath) :: IO (Either SomeException ())
  pool <- runNoLoggingT $ createSqlitePool (T.pack testDbPath) 1
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  pure App
    { appSettings = testSettings
    , appConnPool = pool
    , appMailSend = \_ -> pure ()
    }

-- | Run a database action within a yesod test
testDB :: SqlPersistT (NoLoggingT IO) a -> YesodExample App a
testDB action = do
  app <- getTestYesod
  liftIO $ runNoLoggingT $ runSqlPool action (appConnPool app)
