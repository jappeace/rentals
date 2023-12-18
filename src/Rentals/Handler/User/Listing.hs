module Rentals.Handler.User.Listing where

import Data.Fixed
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Types.Status
import Rentals.Database.Listing
import Rentals.Foundation
import Rentals.Handler.User.Internal
import Rentals.JSON
import Yesod

postListingQuoteR :: ListingId -> Handler TypedContent
postListingQuoteR lid = do
  (start, end) <- parseJsonBody'
  quote <- getQuote lid start end

  sendResponseStatus status200 $ toEncoding quote
