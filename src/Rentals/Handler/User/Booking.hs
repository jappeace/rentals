{-# LANGUAGE RecordWildCards #-}
module Rentals.Handler.User.Booking where

import Yesod
import Rentals.Foundation

import Rentals.Handler.User.Internal

import Text.Blaze(Markup)
import Rentals.Settings

import Rentals.Database.Listing
import Rentals.Database.Source
import Rentals.Database.Event
import Rentals.Database.Checkout
import Rentals.Database.Money
import           Control.Monad
import           Data.Aeson                                  (Result(..), fromJSON)
import qualified Data.Aeson.KeyMap                           as A
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Data.Time.Calendar
import qualified Data.UUID                                   as UUID
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.Mail.Mime
import qualified StripeAPI                                   as Stripe
import           System.Random
import           Text.Blaze.Html.Renderer.Text
import Data.Foldable (for_)
import Rentals.Currency (toStripe, printCurrency)
import Rentals.Widget

data BookListForm = BookListForm {
    startDate :: Day
  , endDate :: Day
 }

type Form a = Markup -> MForm Handler (FormResult a, Widget)

boookListForm :: Form BookListForm
boookListForm csrf = do
  (startRes, startView) <- mreq dayField "start date" Nothing
  (endRes, endView) <- mreq dayField "end date" Nothing

  let view = $(widgetFile "listing/bookform")

      result = BookListForm <$> startRes <*> endRes
  pure (result, view)


getListingBookR :: ListingId -> Handler Html
getListingBookR lid = do
  listing <- runDB $ get404 lid
  (form, enc) <- generateFormPost boookListForm
  mmsg <- getMessage

  userLayoutNoJs $(widgetFile "listing/book")

postListingBookR :: ListingId -> Handler Html
postListingBookR lid = do
  ((formResult, _form), _enc) <- runFormPost boookListForm

  case formResult of
    FormSuccess (BookListForm start end) -> do
      when (start >= end) $ do
        setMessage "Start date must be before end date."
        redirect (ListingBookR lid)

      when (length [start .. end] < 3) $ do
        setMessage "The minimum booking duration is 3 days."
        redirect (ListingBookR lid)

      conflicts <- runDB $ selectList
        ( [EventListing ==. lid, EventStart >=. start, EventStart <=. end, EventBlocked ==. True]
        ||. [EventListing ==. lid, EventStart >=. start, EventStart <=. end, EventBooked ==. True]
        ) []
      unless (null conflicts) $ do
        setMessage "Some of your selected dates are already booked. Please choose different dates."
        redirect (ListingBookR lid)

      listing <- runDB $ get404 lid
      (quote, cleaningFee) <- getQuote lid start end
      stripeKeys <- getsYesod $ appStripe . appSettings
      render     <- getUrlRender
      let amount = 100 * ((floor . unMoney $ quote) + (floor . unMoney $ cleaningFee))

      let conf = Stripe.defaultConfiguration
            { Stripe.configSecurityScheme = Stripe.basicAuthenticationSecurityScheme Stripe.BasicAuthenticationData
              { Stripe.basicAuthenticationDataUsername = stripeSecret stripeKeys
              , Stripe.basicAuthenticationDataPassword = ""
              }
            }

      productResponse <- liftIO . Stripe.runWithConfiguration conf . Stripe.postProducts
        $ (Stripe.mkPostProductsRequestBody $ listingTitle listing)
          { Stripe.postProductsRequestBodyMetadata = Just $ A.fromList [("start", toJSON start), ("end", toJSON end)] }
      product' <- case responseBody productResponse of
        Stripe.PostProductsResponse200 p -> pure p
        Stripe.PostProductsResponseDefault err -> error $ "Stripe postProducts error: " <> show err
        Stripe.PostProductsResponseError   err -> error $ "Stripe postProducts error: " <> err

      let checkoutLineItem = Stripe.mkPostCheckoutSessionsRequestBodyLineItems'
            { Stripe.postCheckoutSessionsRequestBodyLineItems'Quantity = Just 1
            , Stripe.postCheckoutSessionsRequestBodyLineItems'PriceData = Just $
              (Stripe.mkPostCheckoutSessionsRequestBodyLineItems'PriceData' (toStripe (listingCurrency listing)) )
                { Stripe.postCheckoutSessionsRequestBodyLineItems'PriceData'Product = Just $ Stripe.productId product'
                , Stripe.postCheckoutSessionsRequestBodyLineItems'PriceData'UnitAmount = Just amount
                }
            }
          checkoutSession = (Stripe.mkPostCheckoutSessionsRequestBody
            ((render $ ListingBookPaymentCancelR lid)  <> "?session_id={CHECKOUT_SESSION_ID}")
            ((render $ ListingBookPaymentSuccessR lid) <> "?session_id={CHECKOUT_SESSION_ID}")
            ){Stripe.postCheckoutSessionsRequestBodyLineItems = Just [checkoutLineItem]
            , Stripe.postCheckoutSessionsRequestBodyMode = Just Stripe.PostCheckoutSessionsRequestBodyMode'EnumPayment
            , Stripe.postCheckoutSessionsRequestBodyCustomerCreation = Just Stripe.PostCheckoutSessionsRequestBodyCustomerCreation'EnumAlways
            , Stripe.postCheckoutSessionsRequestBodyPaymentIntentData = Just $ Stripe.mkPostCheckoutSessionsRequestBodyPaymentIntentData'
              { Stripe.postCheckoutSessionsRequestBodyPaymentIntentData'Description = Just $ "Thank you for your reservation!"
              }
            }

      checkoutSessionResponse <- liftIO . Stripe.runWithConfiguration conf $ Stripe.postCheckoutSessions checkoutSession
      checkout <- case responseBody checkoutSessionResponse of
        Stripe.PostCheckoutSessionsResponse200 c -> pure c
        Stripe.PostCheckoutSessionsResponseDefault err -> error $ "Stripe postCheckoutSessions error: " <> show err
        Stripe.PostCheckoutSessionsResponseError   err -> error $ "Stripe postCheckoutSessions error: " <> err

      case Stripe.checkout'sessionUrl checkout of
        Just (Stripe.NonNull url) -> redirect url
        _ -> do
          setMessage "No redirect URL was provided by the payment provider."
          redirect (ListingBookR lid)

    FormMissing -> do
      setMessage "Please fill in the booking form."
      redirect (ListingBookR lid)
    FormFailure errs -> do
      setMessage $ toHtml $ T.intercalate ", " errs
      redirect (ListingBookR lid)

getListingBookPaymentSuccessR :: ListingId -> Handler TypedContent
getListingBookPaymentSuccessR lid = do
  params      <- reqGetParams <$> getRequest
  master      <- getsYesod appSettings
  mailSend    <- getsYesod appMailSend
  let stripeKeys  = appStripe master
      adminEmails = map adminEmail $ appAdmin master
      appEmail'   = appEmail master
  mlisting    <- runDB $ get lid

  case (params, mlisting) of
    ([(_, checkoutSessionId)], Just listing) -> do
      let conf = Stripe.defaultConfiguration
            { Stripe.configSecurityScheme = Stripe.basicAuthenticationSecurityScheme Stripe.BasicAuthenticationData
              { Stripe.basicAuthenticationDataUsername = stripeSecret stripeKeys
              , Stripe.basicAuthenticationDataPassword = ""
              }
            }

      checkoutSessionResponse <- liftIO . Stripe.runWithConfiguration conf . Stripe.getCheckoutSessionsSessionLineItems $
        Stripe.mkGetCheckoutSessionsSessionLineItemsParameters checkoutSessionId

      items <- case responseBody checkoutSessionResponse of
        Stripe.GetCheckoutSessionsSessionLineItemsResponse200 items -> pure $ Stripe.getCheckoutSessionsSessionLineItemsResponseBody200Data items
        Stripe.GetCheckoutSessionsSessionLineItemsResponseDefault err -> error $ "Stripe getCheckoutSessionsSessionLineItems error: " <> show err
        Stripe.GetCheckoutSessionsSessionLineItemsResponseError   err -> error $ "Stripe getCheckoutSessionsSessionLineItems error: " <> err

      for_ items $ \i -> case Stripe.itemPrice i of
        Just (Stripe.NonNull ip) -> do
          product' <- case Stripe.itemPrice'NonNullableProduct ip of
            Just (Stripe.ItemPrice'NonNullableProduct'Text           p) -> do
              productResponse <- liftIO . Stripe.runWithConfiguration conf . Stripe.getProductsId $ Stripe.mkGetProductsIdParameters p

              case responseBody productResponse of
                Stripe.GetProductsIdResponse200 products -> pure $ Stripe.productMetadata products
                Stripe.GetProductsIdResponseDefault err -> error $ "Stripe getProductsId error: " <> show err
                Stripe.GetProductsIdResponseError   err -> error $ "Stripe getProductsId error: " <> err

            Just (Stripe.ItemPrice'NonNullableProduct'Product        p) -> pure $ Stripe.productMetadata p
            Just (Stripe.ItemPrice'NonNullableProduct'DeletedProduct p) -> error $ "Stripe product was deleted: " <> T.unpack (Stripe.deletedProductId p)
            other -> error $ "Unxpected " <> show other

          let dates = map fromJSON $ A.elems product'
          case dates of
            [] -> error "Empty dates"
            [Success start', Success end'] -> do
              let (start, end) = if start' > end' then (end', start') else (start', end')

              for_ [start .. end] $ \d -> do
                uuid <- UUID.toText <$> liftIO randomIO
                runDB . flip upsert [EventBooked =. True] $ Event lid Local uuid
                  d end Nothing Nothing Nothing False True
              uuid  <- UUID.toText <$> liftIO randomIO
              uuid' <- UUID.toText <$> liftIO randomIO

              confirmBody <- defaultEmailLayout $(whamletFile "templates/email/book-confirm.hamlet")
              _ <- runDB $ do
                _ <- flip upsert [EventBlocked =. True] $ Event lid Local uuid
                  (pred start) (pred start) Nothing Nothing (Just "Unavailable (Local)") True False
                flip upsert [EventBlocked =. True] $ Event lid Local uuid'
                  (succ end) (succ end) Nothing Nothing (Just "Unavailable (Local)") True False

              checkoutSessionResponse' <- liftIO . Stripe.runWithConfiguration conf . Stripe.getCheckoutSessionsSession $
                Stripe.mkGetCheckoutSessionsSessionParameters checkoutSessionId
              case responseBody checkoutSessionResponse' of
                Stripe.GetCheckoutSessionsSessionResponse200 checkoutSession ->
                  case Stripe.checkout'sessionCustomerDetails checkoutSession of
                    Just (Stripe.NonNull customerDetails) ->
                      case (Stripe.checkout'sessionCustomerDetails'NonNullableEmail customerDetails
                           ,Stripe.checkout'sessionCustomerDetails'NonNullableName  customerDetails) of
                        (Just (Stripe.NonNull customerEmail), Just (Stripe.NonNull customerName)) -> runDB $ do
                          mevent <- getBy $ UniqueEvent lid start
                          case mevent of
                            Just (Entity eid _) -> do

                              _ <- insertUnique_ $ Checkout lid eid checkoutSessionId customerName customerEmail False

                              liftIO $ mailSend $ (emptyMail (Address Nothing appEmail'))
                                { mailTo      = [Address Nothing customerEmail]
                                , mailHeaders = [("Subject", "Booking confirmed - " <> listingTitle listing)]
                                , mailParts   = [[htmlPart $ renderHtml confirmBody]]
                                }
                            Nothing -> sendResponseStatus status500 $ toEncoding
                              ("An error has ocurred: no reservation found at " <> showGregorian start)
                        _ -> error "Stripe checkout session: no email was provided"
                    _ -> error "Stripe checkout session: no customer details provided"
                Stripe.GetCheckoutSessionsSessionResponseDefault err -> error $ "Stripe getCheckoutSessionsSession error: " <> show err
                Stripe.GetCheckoutSessionsSessionResponseError   err -> error $ "Stripe getCheckoutSessionsSession error: " <> err

              emailBody <- defaultEmailLayout $(whamletFile "templates/email/book-alert.hamlet")
              for_ adminEmails $ \adminEmail' -> liftIO $ mailSend $ (emptyMail (Address Nothing appEmail'))
                { mailTo      = [Address Nothing adminEmail']
                , mailHeaders = [("Subject", "New booking - " <> listingTitle listing)]
                , mailParts   = [[htmlPart $ renderHtml emailBody]]
                }
            other -> error $ "Unxpected" <> show other
        other -> error $ "Unxpected" <> show other


      redirect ViewBookSuccessR

    (_, Nothing) -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)
    _ -> sendResponseStatus status400 $ toEncoding
      ("The payment processor did not provide a proper redirect address, missing <session_id> parameter" :: Text)

-- | Stripe redirects here when the user cancels payment
getListingBookPaymentCancelR :: ListingId -> Handler Html
getListingBookPaymentCancelR lid = do
  setMessage "Payment was cancelled."
  redirect (ListingBookR lid)
