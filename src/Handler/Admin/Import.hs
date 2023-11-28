{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.Import where

import Foundation
import Yesod

import Utils

import           Control.Lens               ((^.), to)
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson                 (Result(..))
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

putAdminListingImportR :: ListingId -> Handler TypedContent
putAdminListingImportR lid = do
  requestBody <- parseCheckJsonBody

  case requestBody of
    Success (source, uri) -> do
      mCalendar <- runDB . getBy $ UniqueCalendar lid
      case mCalendar of
        Just (Entity cid _) -> do
          -- Try and parse the provided Airbnb calendar URI
          case URI.parseURI . T.unpack $ uri of
            Just l -> do
              ics <- liftIO . W.get $ T.unpack uri

              let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
              case parseICalendar def parseErrorLog $ ics ^. W.responseBody of
                Right _ -> do
                  runDB . insert $ Import cid source l
                  sendResponseStatus status204 ()

                Left err -> do
                  liftIO $ appendFile "logs/ical-errors" $ "\n" <> err
                  sendResponseStatus status503 $ toEncoding
                    ("The retrieved iCalendar is malformed and could not be parsed: " <> T.pack err)

            Nothing -> sendResponseStatus status400 $ toEncoding
              ("The provided URI is invalid, please check the provided URI and try again" :: Text)

        Nothing -> sendResponseStatus status404 $ toEncoding
          ("The target listing does not exist, please check the identifier and try again" :: Text)

    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)

deleteAdminListingImportR :: ListingId -> Handler TypedContent
deleteAdminListingImportR lid = do
  requestBody <- parseCheckJsonBody

  case requestBody of
    Success source -> do
      runDB $ do
        mCalendar <- getBy $ UniqueCalendar lid

        case mCalendar of
          Just (Entity cid _) -> deleteBy $ UniqueImport cid source
          Nothing -> sendResponseStatus status404 $ toEncoding
            ("The target listing does not exist, please check the identifier and try again" :: Text)

      sendResponseStatus status204 ()

    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)
