module Handler.View.Listings where

import Foundation
import Yesod

import Utils

import           Control.Monad
import           Data.List         ((\\))
import qualified Data.List.GroupBy as GB
import           Data.Maybe
import           Data.Time.Clock
import           Data.Traversable
import           Text.Hamlet
import           Text.Julius

getViewListingsR :: Handler Html
getViewListingsR = do
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

getViewListingR :: Slug -> Handler Html
getViewListingR slug = do
  (Entity lid listing, images, unavailableDates) <- runDB $ do
    listing@(Entity lid _) <- getBy404 $ UniqueSlug slug
    Entity cid _           <- getBy404 $ UniqueCalendar lid
    images                 <- selectList [ListingImageListing ==. lid] []
    unavailableDates       <- map (eventStart . entityVal) <$> selectList
      (   [EventCalendar ==. cid, EventBlocked ==. True]
      ||. [EventCalendar ==. cid, EventBooked  ==. True]
      ) []

    today <- utctDay <$> liftIO getCurrentTime
    let availableDates    = (take 366 [today ..]) \\ unavailableDates
        unavailableDates' = mconcat . filter (\x -> length x < 3) $
          GB.groupBy (\x y -> succ x == y) availableDates

    pure
      ( listing
      , map (listingImageUuid . entityVal) images
      , unavailableDates <> unavailableDates'
      )

  defaultUserLayout $ do
    toWidgetHead $(juliusFile "templates/script/user/datepicker.julius")
    $(whamletFile "templates/user/listing.hamlet")
