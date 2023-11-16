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
import           Data.Text                  (Text)
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
        s | s >= 500 -> do
            sendResponseStatus status500 $ toEncoding
              ("The source for the provided URI is currently unavailable, please try again later" :: Text)
          | s >= 400 -> do
            sendResponseStatus status400 $ toEncoding
              ("Failed to import iCalendar, please check the provided URI and try again" :: Text)
        200 -> do
          eical <- parseCalendar $ ics ^. W.responseBody
          case eical of
            Right ical -> do
              runDB $ do
                mcalendar <- getBy $ UniqueListing lid
                -- merges the events in the calendar already stored
                -- with the events from the incoming calendar
                case mcalendar of
                  Just (Entity cid calendar) -> do
                    let mergedEvents    = M.union
                          (vcEvents $ calendarCalendar calendar)
                          (vcEvents ical)
                    let mergedCalendars =
                          (calendarCalendar calendar) {vcEvents = mergedEvents}

                    update cid [CalendarCalendar =. mergedCalendars]
                    sendResponseStatus status204 ()

                  Nothing -> sendResponseStatus status500 $ toEncoding
                    ("Listing calendar not found, this should not happen" :: Text)

            Left err -> do
              sendResponseStatus status500 $ toEncoding
                ("Failed to process iCalendar, it is malformed: " <> err)

        s -> do
          sendResponseStatus status500 $ toEncoding
            ("An error has ocurred: " <> (T.pack $ show s))

    Nothing -> do
      sendResponseStatus status400 $ toEncoding
        ("The provided URI is invalid, please check the provided URI and try again" :: Text)
