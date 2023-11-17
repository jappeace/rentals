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
  runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool -> liftIO $ do
    runResourceT . flip runSqlPool pool $ runMigration migrateAll

    void . forkIO . forever . runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool ->
      liftIO . runResourceT . flip runSqlPool pool $ do
        calendars <- selectList [CalendarImports !=. []] []

        for calendars $ \(Entity cid calendar) ->
          for (calendarImports calendar) $ \import' -> do
            ics <- liftIO . W.get $ (uriToString id import') ""
            let eical = fmap (head . fst) . parseICalendar def "." $ ics ^. W.responseBody
            sequence_ $ eical <&> \ical -> do
              let mergedEvents    = M.union
                    (vcEvents $ calendarCalendar calendar)
                    (vcEvents ical)
              let mergedCalendars =
                    (calendarCalendar calendar) {vcEvents = mergedEvents}

              update cid [CalendarCalendar =. mergedCalendars]

        liftIO . delay $ 60 * 60 * 1000 * 1000

    warp (appPort settings) $ App settings pool
