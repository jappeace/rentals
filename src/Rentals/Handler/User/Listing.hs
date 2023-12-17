module Rentals.Handler.User.Listing where

import           Rentals.Foundation
import           Yesod

import           Rentals.Handler.User.Internal
import           Rentals.Utils

import           Data.Fixed
import           Data.Maybe
import           Data.List (foldl')
import           Data.Text (Text)
import           Network.HTTP.Types.Status

postListingQuoteR :: ListingId -> Handler TypedContent
postListingQuoteR lid = do
  (start, end) <- parseJsonBody'
  quote <- getQuote lid start end

  sendResponseStatus status200 $ toEncoding quote
