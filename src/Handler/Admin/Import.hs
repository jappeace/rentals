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
      runDB $ update cid [CalendarImports =. (l : calendarImports calendar)]
      sendResponseStatus status204 ()

    Nothing -> do
      sendResponseStatus status400 $ toEncoding
        ("The provided URI is invalid, please check the provided URI and try again" :: Text)
