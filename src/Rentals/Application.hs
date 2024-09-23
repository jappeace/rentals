{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This hooks everything together
module Rentals.Application where

import Control.Monad.Logger (runStderrLoggingT)
import Data.List (uncons)
import Data.Text(Text)
import qualified Data.Text as T
import qualified System.Cron as Cron
import Database.Persist.Postgresql
import Network.HTTP.Types.Method
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Rentals.Foundation
import Rentals.Handler.Admin.Export
import Rentals.Handler.Admin.Import
import Rentals.Handler.Admin.Listing
import Rentals.Handler.User.Booking
import Rentals.Handler.User.Listing
import Rentals.Handler.View.Admin
import Rentals.Handler.View.Listings
import Rentals.Handler.View.Booking
import Rentals.Settings
import System.Directory
import Yesod
import Yesod.Auth
import Yesod.Default.Config2
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Rentals.Database.Migration(runMigrations)
import Network.Mail.Pool (smtpPool)
import Network.Mail.Pool (defSettings)
import Rentals.ICallImporter(importIcall)

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

  smtpPool' <- liftIO $ smtpPool $ defSettings (appSmtpCreds settings)
  runStderrLoggingT $ $logInfo "setting up wai app"
  waiApp <- toWaiApp (App
      { appSettings = settings
      , appConnPool = pool
      , appSmtpPool = smtpPool'
      })
  runStderrLoggingT $ $logInfo $ "binding to port " <> T.pack (show (appPort settings))
  run (appPort settings) $
    ( cors $ \_req ->
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
