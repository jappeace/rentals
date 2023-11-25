module Handler.Admin.Listing where

import Foundation
import Yesod

import Utils

import           Data.List.Extra           (wordsBy)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Time.Calendar
import           Data.Traversable
import           Database.Persist.Sql
import           Network.HTTP.Types.Status
import           System.Random
import           Text.ICalendar
import           Text.Julius
import           Text.Lucius
import           Text.Slugify

getAdminListingR :: ListingId -> Handler Html
getAdminListingR lid = do
  l <- runDB $ get404 lid
  calendars <- runDB $ selectList [CalendarListing ==. lid] []
  let sources = [Airbnb ..]

  let blockedDates = toJSON . mconcat . mconcat . for calendars $ \(Entity _ c) ->
        for (M.elems . vcEvents $ calendarCalendar c) $ \e ->
          case (veDTStart e, veDTEndDuration e) of
            (Just (DTStartDate (Date start) _), Just (Left (DTEndDate (Date end) _))) ->
              [start .. end]
            (Just (DTStartDate (Date start) _), _) ->
              [start]
            _ -> []

  defaultLayout $ do
    toWidgetHead $(juliusFile "templates/script/admin-datepicker.julius")
    toWidgetHead $(luciusFile "templates/style/admin.lucius")
    $(whamletFile "templates/admin/listing.hamlet")

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
  day  <- parseCheckJsonBody
  ct   <- liftIO getCurrentTime
  uuid <- liftIO randomIO
  evn  <- newVEvent ct uuid
    {veDTStart = DTStartDate (Date day) def}

  runDB $ do
    mCal <- getBy $ UniqueImport lid Local
    case mCal of
      Just (Entity cid cal) -> do
        insert_ $ Event cid evn uid' False "Unavailable (Local)"
        update cid [CalendarCalendar =. addVEventToVCalendar (calendarCalendar cal) evn]
      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

putAdminListingUnblockDateR :: ListingId -> Handler TypedContent
putAdminListingUnblockDateR lid = do
  day <- parseCheckJsonBody

  runDB $ do
    mCal <- getBy $ UniqueImport lid Local
    case mCal of
      Just (Entity cid cal) -> do
        update cid [CalendarCalendar =. removeVEventAtDateFromVCalendar (calendarCalendar cal) day]
        deleteBy $ UniqueStart cid day
      Nothing -> Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

postAdminListingNewR :: Handler TypedContent
postAdminListingNewR = do
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
    mcid <- insertUnique $ Calendar lid emptyVCalendar Local Nothing (Just uuid)

    pure (slug, mcid)

  case mcid of
    Just cid -> sendResponseStatus status201 . toEncoding $ unSlug slug
    Nothing  -> sendResponseStatus status500 $ toEncoding
      ("Failed to generate unique identifier, please try again" :: Text)
