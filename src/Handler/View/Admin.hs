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
  (Entity lid l, Entity cid c, is) <- runDB $ do
    l@(Entity lid listing)  <- getBy404 $ UniqueSlug slug
    c@(Entity cid calendar) <- getBy404 $ UniqueCalendar lid
    is                      <- selectList [ImportCalendar ==. cid] []
    pure (l, c, is)

  (blockedDates, usedDates) <- runDB $ do
    bd <- selectList [EventCalendar ==. cid, EventSummary ==. (Just "Unavailable (Local)")] []
    ud <- selectList [EventCalendar ==. cid, EventSummary !=. (Just "Unavailable (Local)")] []
    pure
      ( map (showGregorian . eventStart . entityVal) bd
      , map (showGregorian . eventStart . entityVal) ud
      )

  defaultAdminLayout $ do
    toWidgetHead $(juliusFile "templates/script/admin/datepicker.julius")
    $(whamletFile "templates/admin/listing.hamlet")
