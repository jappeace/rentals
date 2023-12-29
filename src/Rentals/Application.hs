{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This hooks everything together
module Rentals.Application where

import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.Lens (to, (^.))
import Control.Monad
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (def)
import Data.Functor
import Data.List (uncons)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Database.Persist
import Database.Persist.Sqlite
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

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  configPath <- fromMaybe "config/settings.yml" . fmap fst . uncons <$> getArgs
  putStrLn $ "reading config from: " <> configPath
  settings <- loadYamlSettings [configPath] [] useEnv

  createDirectoryIfMissing True "images"
  createDirectoryIfMissing True "config"

  runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool -> liftIO $ do
    runResourceT . flip runSqlPool pool $ runMigration migrateAll

    void $ forkIO $ forever $ runStderrLoggingT $ withSqlitePool "dev.sqlite3" 10 $ \pool -> do
      runResourceT $ flip runSqlPool pool $ do
        imports <- selectList [] []

        for imports $ \(Entity _ (Import cid source uri)) -> do
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

      liftIO . delay $ 60 * 60 * 1000 * 1000

    waiApp <- toWaiApp (App settings pool)
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
