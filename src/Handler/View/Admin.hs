module Handler.View.Admin where

import Foundation
import Yesod

import Data.Time.Calendar
import Database.Persist.Sql
import Text.Julius

getViewAdminR :: Handler Html
getViewAdminR = do
  listings <- runDB $ selectList [] [Asc ListingTitle]

  let newListingWidget = $(whamletFile "templates/admin/new-listing.hamlet")
      listingsWidget   = $(whamletFile "templates/admin/listings.hamlet")
  defaultAdminLayout $ do
    $(whamletFile "templates/admin.hamlet")

getViewAdminListingR :: Slug -> Handler Html
getViewAdminListingR slug = do
  let sources = [Airbnb ..]
  (Entity lid l, Entity cid c, is, imgs) <- runDB $ do
    l@(Entity lid listing)  <- getBy404 $ UniqueSlug slug
    c@(Entity cid calendar) <- getBy404 $ UniqueCalendar lid
    is                      <- selectList [ImportCalendar ==. cid] []
    imgs                    <- selectList [ListingImageEvent ==. lid] []
    pure (l, c, is, imgs)

  (blockedDates, bookedDates) <- runDB $ do
    mbd <- selectList [EventCalendar ==. cid, EventBlocked ==. True] []
    bd  <- selectList [EventCalendar ==. cid, EventBlocked ==. False] []
    pure
      ( map (showGregorian . eventStart . entityVal) mbd
      , map (\(Entity _ ev) -> (showGregorian . eventStart $ ev, eventSource ev)) bd
      )

  defaultAdminLayout $ do
    toWidgetHead $(juliusFile "templates/script/admin/datepicker.julius")
    $(whamletFile "templates/admin/listing.hamlet")
