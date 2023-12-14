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
import qualified Data.Text.Lazy                  as LT
import           Data.Traversable
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sqlite
import           Network.URI
import qualified Network.Wreq                    as W
import           System.Directory
import           Text.ICalendar

import Handler.Admin.Listing
import Handler.Admin.Export
import Handler.Admin.Import
import Handler.User.Booking
import Handler.User.Listing
import Handler.User.User
import Handler.View.Admin
import Handler.View.Listings

import Settings

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  settings <- loadYamlSettings ["config/settings.yml"] [] useEnv

  logExists <- doesFileExist "logs/ical-errors"
  when (not logExists) $ do
    createDirectoryIfMissing True "logs"
    writeFile "logs/ical-errors" mempty

  createDirectoryIfMissing True "images"

  runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool -> liftIO $ do
    runResourceT . flip runSqlPool pool $ runMigration migrateAll

    void . forkIO . forever . runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool ->
      liftIO . runResourceT . flip runSqlPool pool $ do
        imports <- selectList [] []

        for imports $ \(Entity _ (Import cid source uri)) -> do
          let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
          ics <- liftIO . W.get $ (uriToString id uri) ""

          case parseICalendar def parseErrorLog $ ics ^. W.responseBody of
            Right (ical:_, _) -> do
              void . flip M.traverseWithKey (vcEvents ical) $ \_ e -> do
                let uuid        = LT.toStrict . uidValue $ veUID e
                    mdates      = case (veDTStart e, veDTEndDuration e) of
                      (Just (DTStartDate (Date start) _), Just (Left (DTEndDate (Date end) _))) -> Just (start, end)
                      (Just (DTStartDate (Date start) _), _) -> Just (start, start)
                      _ -> Nothing
                    description = fmap (LT.toStrict . descriptionValue) $ veDescription e
                    summary     = fmap (LT.toStrict . summaryValue) $ veSummary e

                case mdates of
                  Just (start, end) -> do
                    void . for [start .. end] $ \start' ->
                      insertBy $ Event cid source uuid
                        start' end Nothing description summary False True
                  Nothing -> pure ()

            Left err ->
              liftIO $ appendFile "logs/ical-errors" $ "\n" <> err

        liftIO . delay $ 60 * 60 * 1000 * 1000

    warp (appPort settings) $ App settings pool
