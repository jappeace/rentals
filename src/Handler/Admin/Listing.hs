module Handler.Admin.Listing where

import Foundation
import Yesod

import Utils

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.Persist.Sql
import           Network.HTTP.Types.Status
import           System.Random
import           Text.Slugify

getAdminListingR :: ListingId -> Handler Html
getAdminListingR lid = do
  mlisting <- runDB $ get lid
  defaultLayout $(whamletFile "templates/admin/listing.hamlet")

postAdminNewListingR :: Handler TypedContent
postAdminNewListingR = do
  listing <- runInputPost $ Listing
    <$> ireq textField "title"
    <*> (unTextarea <$> ireq textareaField "description")
    <*> (realToFrac <$> ireq doubleField "price")
    <*> pure (Slug "")

  (slug, mcid) <- runDB $ do
    lid  <- insert listing

    let slug = Slug . slugify $ (listingTitle listing)
          <> " " <> (T.pack . show $ fromSqlKey lid)
    update lid [ListingSlug =. slug]

    uuid <- liftIO randomIO
    mcid <- insertUnique $ Calendar lid emptyVCalendar [] uuid

    pure (slug, mcid)

  case mcid of
    Just cid -> sendResponseStatus status201 . toEncoding $ unSlug slug
    Nothing  -> sendResponseStatus status500 $ toEncoding
      ("Failed to generate unique identifier, please try again" :: Text)
