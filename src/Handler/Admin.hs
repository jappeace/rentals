module Handler.Admin where

import Foundation
import Yesod

getAdminR :: Handler Html
getAdminR = error "todo"

getViewAdminR :: Handler Html
getViewAdminR = do
  listings <- runDB $ selectList [] [Asc ListingTitle]

  let newListing     = $(whamletFile "templates/admin/new-listing.hamlet")
      listingsWidget = $(whamletFile "templates/admin/listings.hamlet")
  defaultLayout $(whamletFile "templates/admin.hamlet")

getViewAdminListingR :: Handler Html
getViewAdminListingR = error "todo"

postAdminR :: Handler Html
postAdminR = error "todo"
