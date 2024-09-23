module Rentals.JSON(parseJsonBody', ListingNew(..)) where

import Yesod
import Rentals.Foundation
import Rentals.Database.Money

import           Data.Aeson                (Result(..))
import           Data.Aeson.TH             (unwrapUnaryRecords, defaultOptions, deriveJSON)
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status

parseJsonBody' :: FromJSON a => Handler a
parseJsonBody' = do
  requestBody <- parseCheckJsonBody
  case requestBody of
    Success v -> pure v
    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)

data ListingNew = ListingNew
  { listingNewTitle        :: Text
  , listingNewDescription  :: Text
  , listingNewPrice        :: Money
  , listingNewCleaning     :: Money
  , listingNewCountry      :: Text
  , listingNewAddress      :: Text
  , listingNewHandlerName  :: Text
  , listingNewHandlerPhone :: Text
  }
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''ListingNew)
