module Handler.Admin.Export where

import Foundation
import Yesod

import           Data.Text                  (Text)
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Network.HTTP.Types.Status
import           Text.ICalendar

getCalendarExportR :: UUID -> Handler VCalendar
getCalendarExportR uuid = do
  ical <- runDB $ selectList [CalendarUuid ==. Just uuid] []
  case ical of
    [Entity _ c] -> pure $ calendarCalendar c
    [] -> sendResponseStatus status404 $ toEncoding
      ("No ical found for the given UUID, please check if it's correct." :: Text)
    _  -> sendResponseStatus status500 $ toEncoding
      ("More than one icalendar found for the given UUID." :: Text)
