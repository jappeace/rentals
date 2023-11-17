module Handler.Admin where

import Foundation
import Yesod

getAdminR :: Handler Html
getAdminR = do
  listings <- runDB $ selectList [] [Asc ListingTitle]

  let newListing     = $(whamletFile "templates/admin/new-listing.hamlet")
      listingsWidget = $(whamletFile "templates/admin/listings.hamlet")
  defaultLayout $(whamletFile "templates/admin.hamlet")

postAdminR :: Handler Html
postAdminR = error "todo"
