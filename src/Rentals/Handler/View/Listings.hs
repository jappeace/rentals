module Rentals.Handler.View.Listings where

import Rentals.Currency
import Rentals.Foundation
import Yesod
import Rentals.Database.Listing
import Rentals.Database.ListingImage
import Rentals.Database.Event

import           Data.Traversable

getViewListingsR :: Handler Html
getViewListingsR = do
  listings <- runDB $ do
    listings <- selectList [] []

    for listings $ \l@(Entity lid listing) -> do
      image <- selectFirst [ListingImageListing ==. lid] []
      price <- do
        mevent <- selectFirst [EventListing ==. lid, EventBooked ==. False, EventPrice !=. Nothing] [Asc EventPrice]
        case mevent of
          Just (Entity _ event) ->
            case eventPrice event of
              Just price -> pure $ min price (listingPrice listing)
              Nothing -> pure $ listingPrice listing
          Nothing -> pure $ listingPrice listing

      pure (l, price, fmap (listingImageUuid . entityVal) image)

  defaultUserLayout $(whamletFile "templates/user/listings.hamlet")

getViewListingR :: ListingId -> Slug -> Handler Html
getViewListingR lid _slug = do
  (listing, images) <- runDB $ do
    listing <- get404 lid
    images  <- selectList [ListingImageListing ==. lid] []
    pure
      ( listing
      , map (listingImageUuid . entityVal) images
      )

  defaultUserLayout $(whamletFile "templates/user/listing.hamlet")
