module Handler.Admin.Export where

import Foundation
import Yesod

import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Text.ICalendar

getExportCalendarR :: UUID -> Handler VCalendar
getExportCalendarR uuid = do
  Entity _ calendar <- runDB . getBy404 $ UniqueCalendar uuid
  pure $ calendarCalendar calendar
