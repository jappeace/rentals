module Rentals.Handler.View.Admin where

import Rentals.Foundation
import Yesod

import Rentals.Database.Listing
import Rentals.Database.Event
import Rentals.Database.Checkout
import Rentals.Database.Import
import Rentals.Database.Source
import Rentals.Database.ListingImage
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

getViewAdminListingEmailsR :: ListingId -> Slug -> Handler Html
getViewAdminListingEmailsR lid _slug = do
  mlisting      <- runDB $ get lid
  pendingEmails <- runDB $ do
    pendingEmails <- selectList [CheckoutListing ==. lid, CheckoutEmailed ==. False] []
    for pendingEmails $ \ce@(Entity _ c) -> do
      event <- get404 $ checkoutEvent c
      pure (ce, eventStart event, eventEnd event)

  defaultAdminLayout $(whamletFile "templates/admin/compose-email.hamlet")
