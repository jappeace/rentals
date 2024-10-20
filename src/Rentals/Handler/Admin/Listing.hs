module Rentals.Handler.Admin.Listing where

import Rentals.Foundation
import Yesod

import Rentals.JSON
import Rentals.Settings

import Rentals.Database.Listing
import Rentals.Database.Source
import Rentals.Database.Money
import Rentals.Database.Event
import Rentals.Database.Checkout
import Rentals.Database.ListingImage
import           Control.Monad
import           Data.Text                 (Text)
import           Data.Time.Calendar
import           Data.Traversable
import           Data.UUID
import           Network.HTTP.Types.Status
import           Network.Mail.Mime
import           Network.Mail.Pool
import           System.Directory
import           System.FilePath
import           System.Random
import           Text.Slugify
import Rentals.Currency (Currency(..))

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
    Just original -> do
      listing <- parseJsonBody'

      let slug = Slug . slugify $ listingNewTitle listing

      runDB . replace lid $ original
        { listingTitle            = listingNewTitle listing
        , listingDescription      = listingNewDescription listing
        , listingPrice            = listingNewPrice listing
        , listingCleaning         = listingNewCleaning listing
        , listingCountry          = listingNewCountry listing
        , listingAddress          = listingNewAddress listing
        , listingHandlerName      = listingNewHandlerName listing
        , listingHandlerPhone     = listingNewHandlerPhone listing
        , listingSlug             = slug
        }

      sendResponseStatus status200 $ toEncoding slug

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

getImageR :: UUID -> Handler TypedContent
getImageR uuid = sendFile "image/webm" $ "images" </> toString uuid

putAdminListingImageR :: ListingId -> Handler TypedContent
putAdminListingImageR lid = do
  imgFiles <- lookupFiles "images"
  imgs     <- for imgFiles $ \imgFile -> do
    uuid <- liftIO randomIO
    liftIO $ fileMove imgFile ("images" </> toString uuid)

    exists' <- liftIO . doesFileExist $ "images" </> toString uuid
    if exists'
      then do
        runDB . insert_ $ ListingImage lid uuid
        pure uuid
      else
        pure nil

  case filter (/= nil) imgs of
    [] -> sendResponseStatus status400 $ toEncoding
      ("No images found under they key <images>." :: Text)
    x | length x /= length imgs -> sendResponseStatus status500 $ toEncoding
      ("An error has occurred and the image could not be stored." :: Text)
      | otherwise -> sendResponseStatus status201 $ toEncoding imgs

deleteAdminListingImageR :: ListingId -> Handler TypedContent
deleteAdminListingImageR _lid = do
  uuid <- parseJsonBody'
  let file = "images/" </> toString uuid

  exists' <- liftIO $ doesFileExist file
  when exists' $ do
    runDB . deleteBy $ UniqueImage uuid
    liftIO $ removeFile file

  sendResponseStatus status204 ()

putAdminListingUpdateBlockedDatesR :: ListingId -> Handler TypedContent
putAdminListingUpdateBlockedDatesR lid = do
  days <- parseJsonBody' :: Handler [Day]

  _ <- runDB $ do
    mlisting <- get lid

    case mlisting of
      Just _listing -> do
        updateWhere [EventListing ==. lid, EventStart /<-. days] [EventBlocked =. False]

        for days $ \day -> do
          uuid <- toText <$> liftIO randomIO
          flip upsert [EventBlocked =. True] $ Event lid Local uuid
            day day Nothing Nothing (Just "Unavailable (Local)") True False

      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

  sendResponseStatus status204 ()

putAdminListingNewR :: Handler TypedContent
putAdminListingNewR = do
  listing <- parseJsonBody'

  (mlid, slug) <- runDB $ do
    uuid <- liftIO randomIO
    let slug = Slug . slugify $ listingNewTitle listing

    mlid <- insertUnique $ Listing {
          listingTitle            =     (listingNewTitle listing)
        , listingCurrency = UsDollar
        , listingDescription      =     (listingNewDescription listing)
        , listingPrice            =     (listingNewPrice listing)
        , listingCleaning         =     (listingNewCleaning listing)
        , listingCountry          =     (listingNewCountry listing)
        , listingAddress          =     (listingNewAddress listing)
        , listingHandlerName      =     (listingNewHandlerName listing)
        , listingHandlerPhone     =     (listingNewHandlerPhone listing)
        , listingSlug             =     slug
        , listingUuid = uuid
        }


    pure (mlid, slug)

  case mlid of
    Just lid -> do
      render <- getUrlRender
      sendResponseStatus status201 . toEncoding $
        (render $ ViewAdminListingR lid slug)

    Nothing  -> sendResponseStatus status500 $ toEncoding
      ("Failed to generate unique identifier, please try again" :: Text)

putAdminListingUpdateDayPriceR :: ListingId -> Handler TypedContent
putAdminListingUpdateDayPriceR lid = do
  (price, days) <- parseJsonBody' :: Handler (Money, [Day])
  let price' = if price == 0 then Nothing else Just price

  _ <- runDB $ do
    mlisting <- get lid
    case mlisting of
      Just _ -> do
        for days $ \day -> do
          uuid <- toText <$> liftIO randomIO
          flip upsert [EventPrice =. price'] $ Event lid Local uuid
            day day price' Nothing Nothing False False

      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

  sendResponseStatus status204 ()

putAdminListingEmailsR :: ListingId -> Handler TypedContent
putAdminListingEmailsR _lid = do
  (cid, body) <- parseJsonBody'
  mcheckout <- runDB $ get cid

  case mcheckout of
    Just checkout -> do
      master <- getYesod
      let appEmail' = appEmail $ appSettings master
      sendEmail (appSmtpPool master) $ (emptyMail (Address Nothing appEmail'))
        { mailTo      = [Address Nothing $ checkoutEmail checkout]
        , mailHeaders = [("Subject", "Booking Instructions")]
        , mailParts   = [[plainPart body]]
        }

      runDB $ update cid [CheckoutEmailed =. True]
      sendResponseStatus status204 ()

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target checkout does not exist, please check the identifier and try again" :: Text)
