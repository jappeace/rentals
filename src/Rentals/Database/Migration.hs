{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Upgrading the database without dataloss.
module Rentals.Database.Migration(runMigrations) where

import Rentals.Settings
import NeatInterpolation(text)
import Rentals.Database(migrateAll)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Text(Text)
import qualified Control.Exception.Annotated as Annotated
import Data.Pool (Pool)
import Control.Monad.Reader (ReaderT, lift)
import Database.Persist.Sql (SqlBackend, showMigration, runSqlPool)
import Control.Monad.Logger (LoggingT)
import Rentals.Settings (AppDatabase)
import qualified Database.PostgreSQL.Simple as PGSimple
import qualified Database.PostgreSQL.Simple.Migration as Migration
import qualified Database.PostgreSQL.Simple.Util as Migration
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift(askRunInIO)
import  Database.Persist.Postgresql(createPostgresqlPool)

runMigrations :: AppDatabase -> LoggingT IO (Pool SqlBackend)
runMigrations dbSettings = do
    -- garbage collector will destroy the resources.,
    -- no point in using that with mechanism.
    -- (see the docs from data.pool instead of persistent)
    pool <- createPostgresqlPool connString (poolsize dbSettings)

    connection <- liftIO $  PGSimple.connectPostgreSQL connString
    flip runSqlPool pool $ do
      runMigrationCommands migrations connection
      assertPersistentConsistency
    pure pool
    where
      connString = Text.encodeUtf8 $
                   "host="      <> host dbSettings
                <> " port="     <> (Text.pack . show $ port dbSettings)
                <> " dbname="   <> database dbSettings
                <> " user="     <> user dbSettings
                <> " password=" <> password dbSettings


      migrations = migrationFolder dbSettings
      

runMigrationCommands :: MigrationFolder -> PGSimple.Connection -> (ReaderT SqlBackend (LoggingT IO)) ()
runMigrationCommands (MigrationFolder migrationPath) connection = do
  $logInfo $ "using migration folder " <> migrationPath
  runInIO <- askRunInIO
  initialized <- liftIO $ Migration.existsTable connection "schema_migrations"
  let migrations = if initialized
                   then [Migration.MigrationDirectory $ Text.unpack migrationPath]
                   else [ Migration.MigrationInitialization
                        , Migration.MigrationDirectory $ Text.unpack migrationPath]
  result <- liftIO $ Migration.runMigrations connection (migrationOptions runInIO) migrations
  case result of
    Migration.MigrationError err -> Annotated.throw $ MigrationLibraryError err
    Migration.MigrationSuccess -> pure ()

assertPersistentConsistency :: (ReaderT SqlBackend (LoggingT IO)) ()
assertPersistentConsistency = do
      automigration <- showMigration migrateAll
      lift $ if null automigration
        then $logInfo "migrations are consistent"
        else do
          $logError "migrations are inconsistent"
          $logDebug $ Text.unlines automigration
          liftIO . Annotated.throw $ InconsistentMigrationException automigration

data InconsistentMigrationException = InconsistentMigrationException [Text]
                                      | MigrationLibraryError String

instance Show InconsistentMigrationException where
  show (InconsistentMigrationException e) = Text.unpack $ Text.unlines e

instance Annotated.Exception InconsistentMigrationException where
  displayException (InconsistentMigrationException suggestions) =
    Text.unpack $ errorMessage suggestions
  displayException (MigrationLibraryError str') = Text.unpack $
    let str = Text.pack str' in [text| Migration library error: "$str"|]

errorMessage :: [Text] -> Text
errorMessage suggestions =
      let suggestion = (Text.unlines suggestions)
      in
         [text|
            The Persistent model definitions do not match the database schema.
            If you have just edited the schema, you should write a manual
            migration, using make migration and the suggestion printed here:
            $suggestion

            If you have just added a manual migration, you should
            update the schema.
          |]

migrationOptions :: ((ReaderT SqlBackend (LoggingT IO)) () -> IO ()) -> Migration.MigrationOptions
migrationOptions runInIO = Migration.defaultOptions
                    { Migration.optVerbose = Migration.Verbose
                    , Migration.optLogWriter = \case
                        Left errmsg -> runInIO $ lift $  $logWarn errmsg
                        Right infoMsg -> runInIO $ lift $ $logInfo infoMsg
                    , Migration.optTransactionControl = Migration.TransactionPerStep
                    }
