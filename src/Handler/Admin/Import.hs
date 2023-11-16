{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.Import where

import Foundation
import Yesod

import Utils

import           Control.Lens               ((^.), to)
import           Control.Monad
import           Control.Monad.Except
import qualified Data.ByteString            as BS
import           Data.Either
import           Data.List.Extra            (breakEnd)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LTE
import qualified Data.UUID                  as UUID
import           Database.Persist
import           Network.HTTP.Types.Status
import           Network.URI                as URI
import qualified Network.Wreq               as W
import           System.Random
import           Text.ICalendar

postImportCalendarR :: ListingId -> Handler TypedContent
postImportCalendarR lid = do
  _ <- runDB $ get404 lid
  listing <- runInputPost $ ireq textField "listing"

  -- Try and parse the provided Airbnb calendar URI
  case URI.parseURI . T.unpack $ listing of
    Just l -> do
      -- Hit it up and retrieve the .ics file
      ics <- liftIO . W.get . T.unpack $ listing

      case ics ^. W.responseStatus . W.statusCode of
        x | x >= 500 -> sendResponseStatus status503 (error "todo" :: Html)

          | x >= 400 -> sendResponseStatus status400 (error "todo" :: Html)
        200 -> do
          ical <- parseCalendar $ ics ^. W.responseBody
          runDB $ do
            mcalendar <- getBy $ UniqueListing lid
            case mcalendar of
              Just (Entity cid calendar) -> do
                let mergedEvents    = M.union
                      (vcEvents $ calendarCalendar calendar)
                      (vcEvents ical)
                let mergedCalendars =
                      (calendarCalendar calendar) {vcEvents = mergedEvents}

                Just <$> update cid [CalendarCalendar =. mergedCalendars]
              Nothing -> pure Nothing

          return (error "todo")

        _ -> do
          (error "todo")
    Nothing -> (error "todo")

