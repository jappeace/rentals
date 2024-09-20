{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | deal with importing icall calanders
module Rentals.ICallImporter (importIcall, importThisICall ) where

import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.Exception.Annotated
import Control.Lens (to, (^.))
import Control.Monad
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (def)
import Data.Functor
import Data.List (uncons)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Network.HTTP.Types.Method
import Network.Mail.Pool (defSettings, smtpPool)
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import qualified Network.Wreq as W
import Rentals.Database (migrateAll)
import Rentals.Database.Event
import Rentals.Database.Import
import Rentals.Database.Migration (runMigrations)
import Rentals.Foundation
import Rentals.Handler.Admin.Export
import Rentals.Handler.Admin.Import
import Rentals.Handler.Admin.Listing
import Rentals.Handler.User.Booking
import Rentals.Handler.User.Listing
import Rentals.Handler.User.User
import Rentals.Handler.View.Admin
import Rentals.Handler.View.Booking
import Rentals.Handler.View.Listings
import Rentals.Settings
import qualified System.Cron as Cron
import System.Directory
import System.Environment (getArgs)
import Text.ICalendar
import Yesod
import Yesod.Auth
import Yesod.Default.Config2

importIcall :: ReaderT SqlBackend (LoggingT IO) ()
importIcall = do
    imports <- selectList [] []

    void $ for imports $ \(Entity _ import') -> importThisICall import'

importThisICall :: Import -> ReaderT SqlBackend (LoggingT IO) ()
importThisICall (Import cid source uri) = do
    let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
    ics <- liftIO . W.get $ (uriToString id uri) ""

    case parseICalendar def parseErrorLog $ ics ^. W.responseBody of
        Right (ical : _, _) -> do
            void . flip M.traverseWithKey (vcEvents ical) $ \_ e -> do
                let uuid = LT.toStrict . uidValue $ veUID e
                    mdates = case (veDTStart e, veDTEndDuration e) of
                        (Just (DTStartDate (Date start) _), Just (Left (DTEndDate (Date end) _))) -> Just (start, pred end)
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
