{-# LANGUAGE OverloadedStrings #-}
module Rentals.Handler.Admin.Import where

import Rentals.Foundation
import Yesod

import Rentals.JSON

import Rentals.ICallImporter(importThisICall )
import Rentals.Database.Import
import Rentals.Database.Listing
import           Control.Lens               ((^.), (.~), (&))
import           Data.Aeson                 (Result(..))
import           Data.Default               (def)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.HTTP.Types.Status
import           Network.URI                as URI
import qualified Network.Wreq               as W
import           Text.ICalendar

putAdminListingImportR :: ListingId -> Handler TypedContent
putAdminListingImportR lid = do
  (source, uri) <- parseJsonBody'
  mlisting      <- runDB $ get lid

  case mlisting of
    Just _listing -> do
      -- Try and parse the provided Airbnb calendar URI
      case URI.parseURI . T.unpack $ uri of
        Just l -> do
          let opts = W.defaults & W.checkResponse .~ (Just $ \_ _ -> pure ())
          res <- liftIO . W.getWith opts $ T.unpack uri

          let parseErrorLog = "Failed to parse <" <> show source <> "> ics file: "
          case res ^. W.responseStatus . W.statusCode of
            200 ->
              case parseICalendar def parseErrorLog $ res ^. W.responseBody of
                Right _ -> do
                  let res' = Import lid source l
                  runDB $ do
                    _ <- insert res'
                    importThisICall res'
                  sendResponseStatus status204 ()

                Left err -> do
                  $logError $ "Parsing ical failed: " <> T.pack err
                  sendResponseStatus status503 $ toEncoding
                    ("The retrieved iCalendar is malformed and could not be parsed: " <> T.pack err)

            sc -> sendResponseStatus status400 $ toEncoding
              ("An error has ocurred when trying to retrieve the ics file: " <> show sc)

        Nothing -> sendResponseStatus status400 $ toEncoding
          ("The provided URI is invalid, please check the provided URI and try again" :: Text)

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

deleteAdminListingImportR :: ListingId -> Handler TypedContent
deleteAdminListingImportR lid = do
  requestBody <- parseCheckJsonBody

  case requestBody of
    Success source -> do
      runDB $ do
        mlisting <- get lid

        case mlisting of
          Just _ -> deleteBy $ UniqueImport lid source
          Nothing -> sendResponseStatus status404 $ toEncoding
            ("The target listing does not exist, please check the identifier and try again" :: Text)

      sendResponseStatus status204 ()

    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)
