module Handler.Admin.Listing where

import Foundation
import Yesod

import Utils

import           Data.Text                 (Text)
import           Network.HTTP.Types.Status
import           System.Random

getAdminListingR :: ListingId -> Handler Html
getAdminListingR lid = do
  mlisting <- runDB $ get lid
  defaultLayout $(whamletFile "templates/admin/listing.hamlet")

getAdminNewListingR :: Handler Html
getAdminNewListingR = do
  defaultLayout $(whamletFile "templates/admin/new-listing.hamlet")

postAdminNewListingR :: Handler Html
postAdminNewListingR = do
  listing <- runInputPost $ Listing
    <$> ireq textField "title"
    <*> (unTextarea <$> ireq textareaField "description")
    <*> (realToFrac <$> ireq doubleField "price")

  (lid, mcid) <- runDB $ do
    lid  <- insert listing
    uuid <- liftIO randomIO
    mcid <- insertUnique $ Calendar lid emptyVCalendar uuid
    pure (lid, mcid)

  case mcid of
    Just cid -> do
      redirectWith status201 $ ListingR lid
    Nothing  -> do
      setMessage $ toHtml ("Failed to generate unique identifier, please try again" :: Text)
      redirectWith status500 AdminNewListingR
