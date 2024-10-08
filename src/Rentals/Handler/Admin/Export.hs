module Rentals.Handler.Admin.Export where

import Rentals.Foundation
import Yesod

import           Data.Default               (def)
import           Data.Foldable
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Time.Clock
import qualified Data.UUID                  as UUID
import           Network.HTTP.Types.Status
import           Text.ICalendar
import Rentals.Database.Event
import Rentals.Database.Listing
import Rentals.Database.Source
import Rentals.Calendar


getCalendarExportR :: ICS -> Handler VCalendar
getCalendarExportR (ICS uuid) = do
  mlisting <- runDB . getBy $ UniqueCalendar uuid
  case mlisting of
    Just (Entity lid _listing) -> do
      currentTime <- liftIO getCurrentTime
      events      <- runDB $ selectList [EventListing ==. lid, EventSource ==. Local] []

      let appendEvents vcalendar (Entity _ event) =
            case UUID.fromText (eventUuid event) of
              Just eventUUID ->
                addVEventToVCalendar vcalendar $ (newVEvent currentTime eventUUID)
                  { veDTStart = Just $ DTStartDate (Date (eventStart event)) def
                  , veDTEndDuration = Just . Left $ DTEndDate (Date (succ $ eventEnd event)) def
                  , veDescription = fmap (\d -> Description (LT.fromStrict d) Nothing Nothing def) $ eventDescription event
                  , veSummary = fmap (\d -> Summary (LT.fromStrict d) Nothing Nothing def) $ eventSummary event
                  , veTransp = Opaque def
                  }
              Nothing -> vcalendar

      pure $ foldl' appendEvents emptyVCalendar events

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("No ical found for the given UUID, please check if it's correct" :: Text)
