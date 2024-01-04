{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This hooks everything together
module Rentals.Application where

import Control.Monad.Logger(LoggingT)
import Data.Pool (Pool)
import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.Exception.Annotated
import Control.Lens (to, (^.))
import Control.Monad
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (def)
import Data.Functor
import Data.List (uncons)
import Data.Text(Text)
import Control.Monad.Reader(ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified System.Cron as Cron
import Data.Traversable
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Network.HTTP.Types.Method
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import qualified Network.Wreq as W
import Rentals.Database (migrateAll)
import Rentals.Database.Event
import Rentals.Database.Import
import Rentals.Foundation
import Rentals.Handler.Admin.Export
import Rentals.Handler.Admin.Import
import Rentals.Handler.Admin.Listing
import Rentals.Handler.User.Booking
import Rentals.Handler.User.Listing
import Rentals.Handler.User.User
import Rentals.Handler.View.Admin
import Rentals.Handler.View.Listings
import Rentals.Handler.View.Booking
import Rentals.Settings
import System.Directory
import Text.ICalendar
import Yesod
import Yesod.Auth
import Yesod.Default.Config2
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Rentals.Database.Migration(runMigrations)

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  configPath <- fromMaybe "config/settings.yml" . fmap fst . uncons <$> getArgs
  putStrLn $ "reading config from: " <> configPath
  settings <- loadYamlSettings [configPath] [] useEnv

  createDirectoryIfMissing True "images"
  createDirectoryIfMissing True "config"

  pool <- runStderrLoggingT $ do
    $logInfo "running migrations"
    runMigrations $ appDatabase settings

  runStderrLoggingT $ $logInfo "forking background jobs"

  _tids <- Cron.execSchedule $ do

    Cron.addJob (runStderrLoggingT $ flip runSqlPool pool $ importIcall) everyHour

  runStderrLoggingT $ $logInfo "setting up wai app"
  waiApp <- toWaiApp (App settings pool)
  runStderrLoggingT $ $logInfo $ "binding to port " <> T.pack (show (appPort settings))
  run (appPort settings) $
    ( cors $ \req ->
        Just $
          simpleCorsResourcePolicy
            { corsMethods =
                [ methodOptions,
                  methodGet,
                  methodPost,
                  methodPut,
                  methodDelete
                ]
            }
    )
      waiApp

everyHour :: Text
          -- ┌────────────── minute (0 - 59)
          -- │ ┌───────────── hour (0 - 23)
          -- │ │ ┌───────────── day of the month (1 - 31)
          -- │ │ │ ┌───────────── month (1 - 12 or JAN-DEC)
          -- │ │ │ │ ┌───────────── day of the week (0 - 6 or SUN-SAT)
          -- │ │ │ │ │
          -- │ │ │ │ │
          -- │ │ │ │ │
everyHour = "0 * * * *"

importIcall :: (ReaderT SqlBackend (LoggingT IO)) ()
importIcall = do
          imports <- selectList [] []

          void $ for imports $ \(Entity _ (Import cid source uri)) -> do
            let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
            ics <- liftIO . W.get $ (uriToString id uri) ""

            case parseICalendar def parseErrorLog $ ics ^. W.responseBody of
              Right (ical : _, _) -> do
                void . flip M.traverseWithKey (vcEvents ical) $ \_ e -> do
                  let uuid = LT.toStrict . uidValue $ veUID e
                      mdates = case (veDTStart e, veDTEndDuration e) of
                        (Just (DTStartDate (Date start) _), Just (Left (DTEndDate (Date end) _))) -> Just (start, end)
                        (Just (DTStartDate (Date start) _), _) -> Just (start, start)
                        _ -> Nothing
                      description = fmap (LT.toStrict . descriptionValue) $ veDescription e
                      summary = fmap (LT.toStrict . summaryValue) $ veSummary e

                  case mdates of
                    Just (start, end) -> do
                      void . for [start .. end] $ \start' ->
                        insertBy $
                          Event
                            cid
                            source
                            uuid
                            start'
                            end
                            Nothing
                            description
                            summary
                            False
                            True
                    Nothing -> pure ()
              Left err ->
                $logError $ "Parsing ical failed: " <> T.pack err
