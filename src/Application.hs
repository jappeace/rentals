{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod
import Yesod.Auth
import Yesod.Default.Config2

import           Control.Concurrent
import           Control.Concurrent.Thread.Delay
import           Control.Lens                    ((^.), to)
import           Control.Monad
import           Control.Monad.Logger            (runStderrLoggingT)
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.Default                    (def)
import           Data.Functor
import qualified Data.Map.Strict                 as M
import           Data.Traversable
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sqlite
import           Network.URI
import qualified Network.Wreq                    as W
import           System.Directory
import           Text.ICalendar

import Handler.Admin
import Handler.Admin.Listing
import Handler.Admin.Export
import Handler.Admin.Import
import Handler.Home
import Handler.Listing

import Settings

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  settings <- loadYamlSettings ["config/settings.yml"] [] useEnv

  logExists <- doesFileExist "logs/ical-errors"
  when (not logExists) $ do
    createDirectoryIfMissing True "logs"
    writeFile "logs/ical-errors" mempty

  runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool -> liftIO $ do
    runResourceT . flip runSqlPool pool $ runMigration migrateAll

    void . forkIO . forever . runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool ->
      liftIO . runResourceT . flip runSqlPool pool $ do
        calendars <- selectList [CalendarImportUri !=. Nothing] []

        for calendars $ \(Entity cid calendar) ->
          for (calendarImportUri calendar) $ \(import') -> do
            ics <- liftIO . W.get $ (uriToString id import') ""
            case parseICalendar def "logs/ical-errors" $ ics ^. W.responseBody of
              Right (ical:_, _) ->
                update cid [CalendarCalendar =. ical]
              Left err ->
                liftIO $ appendFile "logs/ical-errors" $ "\n" <> err
        liftIO . delay $ 60 * 60 * 1000 * 1000

    warp (appPort settings) $ App settings pool
