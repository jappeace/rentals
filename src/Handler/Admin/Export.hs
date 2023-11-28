module Handler.Admin.Export where

import Foundation
import Yesod

import           Data.Default               (def)
import           Data.Foldable
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Time.Clock
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Network.HTTP.Types.Status
import           Text.ICalendar
import           System.Random

import Utils

getCalendarExportR :: UUID -> Handler VCalendar
getCalendarExportR uuid = do
  mcalendar <- runDB . getBy $ UniqueUUID uuid
  case mcalendar of
    Just (Entity cid calendar) -> do
      currentTime <- liftIO getCurrentTime
      eventUUID   <- liftIO randomIO
      events      <- runDB $ selectList [EventCalendar ==. cid] []

      let vcalendar = emptyVCalendar
          appendEvents vcalendar (Entity _ event) =
            addVEventToVCalendar vcalendar $ (newVEvent currentTime eventUUID)
              { veDTStart = Just $ DTStartDate (Date (eventStart event)) def
              , veDTEndDuration = Just . Left $ DTEndDate (Date (eventEnd event)) def
              , veDescription = fmap (\d -> Description (LT.fromStrict d) Nothing Nothing def) $ eventDescription event
              , veSummary = fmap (\d -> Summary (LT.fromStrict d) Nothing Nothing def) $ eventSummary event
              , veTransp = if eventOpaque event then Opaque def else Transparent def
              }

      pure $ foldl' appendEvents vcalendar events

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("No ical found for the given UUID, please check if it's correct" :: Text)
