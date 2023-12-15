module Handler.User.Listing where

import           Foundation
import           Yesod

import           Handler.User.Internal
import           Utils

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
