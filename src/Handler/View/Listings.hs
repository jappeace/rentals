module Handler.View.Listings where

import Foundation
import Yesod

import Utils

import Control.Monad
import Data.Maybe
import Data.Traversable
import Text.Hamlet

getListingsR :: Handler Html
getListingsR = do
  listings <- runDB $ do
    listings <- selectList [] []

    for listings $ \l@(Entity lid listing) -> do
      image <- selectFirst [ListingImageListing ==. lid] []
      price <- do
        mCal <- getBy $ UniqueCalendar lid
        case mCal of
          Just (Entity cid _) -> do
            mEvent <- selectFirst [EventCalendar ==. cid, EventBooked ==. False, EventPrice !=. Nothing] [Asc EventPrice]
            case mEvent of
              Just (Entity _ event) ->
                case eventPrice event of
                  Just price -> pure $ min price (listingPrice listing)
                  Nothing -> pure $ listingPrice listing
              Nothing -> pure $ listingPrice listing
          Nothing -> pure $ listingPrice listing

      pure (l, price, fmap (listingImageUuid . entityVal) image)

  defaultUserLayout $(whamletFile "templates/user/listings.hamlet")

getListingR :: Slug -> Handler Html
getListingR slug = do
  undefined
