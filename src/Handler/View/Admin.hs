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
    $(whamletFile "templates/admin.hamlet")

getViewAdminListingR :: Slug -> Handler Html
getViewAdminListingR slug = do
  let sources = [Airbnb ..]
  (Entity lid l, Entity cid c, is, imgs) <- runDB $ do
    l@(Entity lid listing)  <- getBy404 $ UniqueSlug slug
    c@(Entity cid calendar) <- getBy404 $ UniqueCalendar lid
    is                      <- selectList [ImportCalendar ==. cid] []
    imgs                    <- selectList [ListingImageListing ==. lid] []
    pure (l, c, is, imgs)

  (blockedDates, bookedDates, pricedDates) <- runDB $ do
    mbd <- selectList [EventCalendar ==. cid, EventBlocked ==. True] []
    bd  <- selectList [EventCalendar ==. cid, EventBooked  ==. True] []
    pd  <- selectList [EventCalendar ==. cid, EventBooked  ==. False, EventPrice !=. Nothing] []
    pure
      ( map (showGregorian . eventStart . entityVal) mbd
      , map (\(Entity _ ev) -> (showGregorian . eventStart $ ev, eventSource ev)) bd
      , map (\(Entity _ ev) -> (showGregorian . eventStart $ ev, eventPrice  ev)) pd
      )

  defaultAdminLayout $ do
    toWidgetHead $(juliusFile "templates/script/admin/datepicker.julius")
    $(whamletFile "templates/admin/listing.hamlet")
