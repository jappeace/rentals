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

  -- defaultLayout $ do
  --   toWidgetHead $(juliusFile "templates/script/admin-datepicker.julius")
  --   toWidgetHead $(luciusFile "templates/style/admin.lucius")
  --   $(whamletFile "templates/admin/listing.hamlet")

getAdminListingSourcesR :: Handler TypedContent
getAdminListingSourcesR = do
  sendResponseStatus status200 $ toEncoding [Airbnb ..]

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

putAdminListingBlockDateR :: ListingId -> Handler TypedContent
putAdminListingBlockDateR lid = do
  requestBody <- parseCheckJsonBody
  -- ct   <- liftIO getCurrentTime
  -- evn  <- newVEvent ct uuid
  --   {veDTStart = DTStartDate (Date day) def}

  case requestBody of
    Success day -> runDB $ do
      mcalendar <- getBy $ UniqueCalendar lid

      case mcalendar of
        Just (Entity cid _) -> do
          uuid <- toText <$> liftIO randomIO
          insert_ $ Event cid Local uuid
            day day Nothing (Just "Unavailable (Local)") True
          sendResponseStatus status204 ()

        Nothing -> sendResponseStatus status404 $ toEncoding
          ("The target listing does not exist, please check the identifier and try again" :: Text)

    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)

putAdminListingUnblockDateR :: ListingId -> Handler TypedContent
putAdminListingUnblockDateR lid = do
  requestBody <- parseCheckJsonBody

  case requestBody of
    Success day -> runDB $ do
      mcalendar <- getBy $ UniqueCalendar lid
      case mcalendar of
        Just (Entity cid _) -> do
          deleteBy $ UniqueEvent cid day
          sendResponseStatus status204 ()

        Nothing -> sendResponseStatus status404 $ toEncoding
          ("The target listing does not exist, please check the identifier and try again" :: Text)

    Error err -> sendResponseStatus status400 $ toEncoding
      ("Unable to parse the request body: " <> err)

putAdminListingNewR :: Handler TypedContent
putAdminListingNewR = do
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
    mcid <- insertUnique $ Calendar lid uuid

    pure (slug, mcid)

  case mcid of
    Just cid -> sendResponseStatus status201 . toEncoding $ unSlug slug
    Nothing  -> sendResponseStatus status500 $ toEncoding
      ("Failed to generate unique identifier, please try again" :: Text)
