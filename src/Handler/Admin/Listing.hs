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

putAdminListingR :: ListingId -> Handler TypedContent
putAdminListingR lid = do
  mListing <- runDB $ get lid
  case mListing of
    Just _ -> do
      uListing <- runInputPost $ Listing
        <$> ireq textField "title"
        <*> (unTextarea <$> ireq textareaField "description")
        <*> (realToFrac <$> ireq doubleField "price")
        <*> pure (Slug "")

      let slug = Slug . slugify $ (listingTitle uListing)
            <> " " <> (T.pack . show $ fromSqlKey lid)

      runDB . replace lid $ uListing {listingSlug = slug}
      sendResponseStatus status204 ()

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

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
