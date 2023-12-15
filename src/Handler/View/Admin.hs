module Handler.View.Admin where

import Foundation
import Yesod

import Data.Time.Calendar
import Data.Traversable
import Database.Persist.Sql
import Text.Julius

getViewAdminR :: Handler Html
getViewAdminR = do
  listings <- runDB $ do
    listings <- selectList [] [Asc ListingTitle]
    for listings $ \l@(Entity lid _) -> do
      i <- selectFirst [ListingImageListing ==. lid] []
      pure (l, fmap (listingImageUuid . entityVal) i)

  defaultAdminLayout $ do
    $(whamletFile "templates/admin/admin.hamlet")

getViewAdminListingR :: ListingId -> Slug -> Handler Html
getViewAdminListingR lid slug = do
  let sources = [Airbnb ..]
  (l, is, imgs) <- runDB $ do
    l    <- get404 lid
    is   <- selectList [ImportListing ==. lid] []
    imgs <- selectList [ListingImageListing ==. lid] []
    pure (l, is, imgs)

  (blockedDates, bookedDates, pricedDates) <- runDB $ do
    mbd <- selectList [EventListing ==. lid, EventBlocked ==. True] []
    bd  <- selectList [EventListing ==. lid, EventBooked  ==. True] []
    pd  <- selectList [EventListing ==. lid, EventBooked  ==. False, EventPrice !=. Nothing] []
    pure
      ( map (showGregorian . eventStart . entityVal) mbd
      , map (\(Entity _ ev) -> (showGregorian . eventStart $ ev, eventSource ev)) bd
      , map (\(Entity _ ev) -> (showGregorian . eventStart $ ev, eventPrice  ev)) pd
      )

  defaultAdminLayout $ do
    toWidgetHead $(juliusFile "templates/script/admin/datepicker.julius")
    $(whamletFile "templates/admin/listing.hamlet")
