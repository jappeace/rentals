{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.Import where

import Foundation
import Yesod

import Utils

import           Control.Lens               ((^.), to)
import           Control.Monad
import           Control.Monad.Except
import qualified Data.ByteString            as BS
import           Data.Default               (def)
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
  (calendarSource, calendarURI) <- runInputPost $ (,)
    <$> (read . T.unpack <$> ireq textField "calendar-source")
    <*> ireq urlField "calendar-uri"

  runDB $ get404 lid
  -- Try and parse the provided Airbnb calendar URI
  case URI.parseURI . T.unpack $ calendarURI of
    Just l -> do
      ics <- liftIO . W.get $ T.unpack calendarURI
      case parseICalendar def "logs/ical-errors" $ ics ^. W.responseBody of
        Right (ical:_, _) -> do
          uuid <- liftIO randomIO
          runDB . insert $ Calendar lid ical calendarSource (Just l) uuid
          sendResponseStatus status204 ()
        Left err -> do
          liftIO $ appendFile "logs/ical-errors" $ "\n" <> err
          sendResponseStatus status503 $ toEncoding
            ("The retrieved iCalendar is malformed and could not be parsed: " <> T.pack err)

    Nothing ->
      sendResponseStatus status400 $ toEncoding
        ("The provided URI is invalid, please check the provided URI and try again" :: Text)

deleteImportCalendarR :: ListingId -> Handler TypedContent
deleteImportCalendarR lid = do
  sourceURI <- runInputPost (read . T.unpack <$> ireq textField "source-uri")
  runDB . deleteBy $ UniqueImport lid sourceURI
  sendResponseStatus status204 ()
