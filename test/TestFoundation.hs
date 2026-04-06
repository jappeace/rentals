{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, NoLoggingT, logInfo)
import Network.Mail.Pool (SmtpCred(..))
import Network.Mail.Mime (mailTo, mailHeaders, addressEmail)
import Yesod.Test (YesodExample, getTestYesod)
import System.Directory (createDirectoryIfMissing)
import System.IO (openTempFile, hClose)
import Control.Monad.IO.Class (liftIO)
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

-- | Create a test App with a unique SQLite file-based database.
--   Uses the same dev-mode email logging as the real application.
makeTestApp :: IO App
makeTestApp = do
  createDirectoryIfMissing True "config"
  (testDbPath, handle) <- openTempFile "/tmp" "rentals-test-.sqlite3"
  hClose handle
  pool <- runNoLoggingT $ createSqlitePool (T.pack testDbPath) 1
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  pure App
    { appSettings = testSettings
    , appConnPool = pool
    , appMailSend = \mail -> runStderrLoggingT $ do
        let subject = lookup "Subject" (mailHeaders mail)
            recipients = T.intercalate ", " $ map addressEmail (mailTo mail)
        $logInfo $ "EMAIL (dev, not sent) to=[" <> recipients <> "] subject=[" <> maybe "(none)" id subject <> "]"
    }

-- | Run a database action within a yesod test
testDB :: SqlPersistT (NoLoggingT IO) a -> YesodExample App a
testDB action = do
  app <- getTestYesod
  liftIO $ runNoLoggingT $ runSqlPool action (appConnPool app)
