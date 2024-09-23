{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | deal with importing icall calanders
module Rentals.ICallImporter (importIcall, importThisICall ) where


import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Database.Persist
import Database.Persist.Postgresql
import Network.URI
import qualified Network.Wreq as W
import Rentals.Database.Event
import Rentals.Database.Import
import Text.ICalendar
import Yesod

importIcall :: ReaderT SqlBackend (LoggingT IO) ()
importIcall = do
    imports <- selectList [] []

    void $ for imports $ \(Entity _ import') -> importThisICall import'

importThisICall :: (MonadLogger m, MonadIO m) => Import -> ReaderT SqlBackend m ()
importThisICall (Import cid source uri) = do
    let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
    ics <- liftIO . W.get $ (uriToString id uri) ""

    case parseICalendar def parseErrorLog $ ics ^. W.responseBody of
        Right ([], _) -> $logError $ "empty list"
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
