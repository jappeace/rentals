module Handler.Admin.Listing where

import Foundation
import Yesod

import Utils

import           Data.Aeson                (Result(..))
import           Data.List.Extra           (wordsBy)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Time.Calendar
import           Data.Traversable
import           Data.UUID
import           Database.Persist.Sql
import           Network.HTTP.Types.Status
import           System.Random
import           Text.ICalendar
import           Text.Julius
import           Text.Lucius
import           Text.Slugify

getAdminListingR :: ListingId -> Handler TypedContent
getAdminListingR lid = do
  mlisting <- runDB $ get lid

  case mlisting of
    Just listing -> sendResponseStatus status200 listing
    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

getAdminListingSourcesR :: Handler TypedContent
getAdminListingSourcesR = do
  sendResponseStatus status200 $ toEncoding [Airbnb ..]

putAdminListingR :: ListingId -> Handler TypedContent
putAdminListingR lid = do
  mListing <- runDB $ get lid
  case mListing of
    Just _ -> do
      listing <- parseJsonBody'

      let slug = Slug . slugify $ (listingTitle listing)
            <> " " <> (T.pack . show $ fromSqlKey lid)

      runDB . replace lid $ listing {listingSlug = slug}

      sendResponseStatus status200 $ toEncoding slug

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

putAdminListingBlockDateR :: ListingId -> Handler TypedContent
putAdminListingBlockDateR lid = do
  day <- parseJsonBody'

  runDB $ do
    mcalendar <- getBy $ UniqueCalendar lid

    case mcalendar of
      Just (Entity cid _) -> do
        uuid <- toText <$> liftIO randomIO
        insert_ $ Event cid Local uuid
          day day Nothing (Just "Unavailable (Local)") True
        sendResponseStatus status204 ()

      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

putAdminListingUnblockDateR :: ListingId -> Handler TypedContent
putAdminListingUnblockDateR lid = do
  day <- parseJsonBody'

  runDB $ do
    mcalendar <- getBy $ UniqueCalendar lid

    case mcalendar of
      Just (Entity cid _) -> do
        deleteBy $ UniqueEvent cid day
        sendResponseStatus status204 ()

      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

putAdminListingNewR :: Handler TypedContent
putAdminListingNewR = do
  listing <- parseJsonBody'

  (slug, mcid) <- runDB $ do
    lid  <- insert listing

    let slug = Slug . slugify $ (listingTitle listing)
          <> " " <> (T.pack . show $ fromSqlKey lid)
    update lid [ListingSlug =. slug]

    uuid <- liftIO randomIO
    mcid <- insertUnique $ Calendar lid uuid

    pure (slug, mcid)

  case mcid of
    Just cid -> do
      render <- getUrlRender
      sendResponseStatus status201 . toEncoding $
        (render $ ViewAdminListingR slug)
    Nothing  -> sendResponseStatus status500 $ toEncoding
      ("Failed to generate unique identifier, please try again" :: Text)
