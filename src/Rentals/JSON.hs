module Rentals.JSON(parseJsonBody', ListingNew(..)) where

import Yesod
import Rentals.Foundation
import Rentals.Database.Money

import           Data.Aeson                (Result(..), FromJSON)
import           Data.Aeson.TH             (unwrapUnaryRecords, defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy      as LBS
import           Data.CaseInsensitive
import           Data.Default              (def)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.UUID
import           Data.Version
import           Network.HTTP.Types.Status
import           System.Random
import           Text.ICalendar

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
