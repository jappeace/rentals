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
  mCalendar <- runDB . getBy $ UniqueListing lid
  listing <- runInputPost $ ireq textField "listing"

  case mCalendar of
    -- Try and parse the provided Airbnb calendar URI
    Just (Entity cid calendar) -> case URI.parseURI . T.unpack $ listing of
      Just l -> do
        runDB $ update cid [CalendarImports =. (l : calendarImports calendar)]
        sendResponseStatus status204 ()

      Nothing ->
        sendResponseStatus status400 $ toEncoding
          ("The provided URI is invalid, please check the provided URI and try again" :: Text)
    Nothing ->
      sendResponseStatus status404 $ toEncoding
        ("The requested listing does not exist, please check the identifier and try again" :: Text)
