module Rentals.Handler.User.Booking where

import Yesod
import Rentals.Foundation

import Rentals.Handler.User.Internal

import Rentals.Settings
import Rentals.JSON

import Rentals.Database.Listing
import Rentals.Database.Source
import Rentals.Database.Event
import Rentals.Database.Checkout
import Rentals.Database.Money
import           Control.Monad
import           Data.Aeson                                  (Result(..), fromJSON)
import qualified Data.Aeson.KeyMap                           as A
import           Data.List                                   (sort)
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Data.Time.Calendar
import           Data.Traversable
import qualified Data.UUID                                   as UUID
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.Mail.Mime
import           Network.Mail.Pool
import qualified StripeAPI                                   as Stripe
import qualified StripeAPI.Types.NotificationEventData.Extra as Stripe
import           System.Random
import           Text.Blaze.Html.Renderer.Text
import Data.Foldable (for_)

putListingBookR :: ListingId -> Handler TypedContent
putListingBookR lid = do
  (start, end) <- parseJsonBody'
  mlisting     <- runDB $ get lid

  when (length [start .. end] < 3) . sendResponseStatus status400 $ toEncoding
    ("The minimum amount of days for booking is 3." :: Text)

  listing <- case mlisting of
    Just listing -> pure listing
    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

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
  product <- case responseBody productResponse of
    Stripe.PostProductsResponse200 product -> pure product
    Stripe.PostProductsResponseDefault err -> sendResponseStatus status503 $ toEncoding
      ("An error has ocurred with the payment processor: " <> (T.pack $ show err))
    Stripe.PostProductsResponseError   err -> sendResponseStatus status503 $ toEncoding
      ("An error has ocurred with the payment processor: " <> T.pack err)

  let checkoutLineItem = Stripe.mkPostCheckoutSessionsRequestBodyLineItems'
        { Stripe.postCheckoutSessionsRequestBodyLineItems'Quantity = Just 1
        , Stripe.postCheckoutSessionsRequestBodyLineItems'PriceData = Just $
          (Stripe.mkPostCheckoutSessionsRequestBodyLineItems'PriceData' "USD")
            { Stripe.postCheckoutSessionsRequestBodyLineItems'PriceData'Product = Just $ Stripe.productId product
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
    Stripe.PostCheckoutSessionsResponse200 checkout -> pure checkout
    Stripe.PostCheckoutSessionsResponseDefault err -> sendResponseStatus status503 $ toEncoding
      ("An error has ocurred with the payment processor: " <> (T.pack $ show err))
    Stripe.PostCheckoutSessionsResponseError   err -> sendResponseStatus status503 $ toEncoding
      ("An error has ocurred with the payment processor: " <> T.pack err)

  case Stripe.checkout'sessionUrl checkout of
    Just (Stripe.NonNull url) -> sendResponseStatus status200 $ toEncoding url
    _ -> sendResponseStatus status503 $ toEncoding
      ("No redirect URL was provided by the payment provider" :: Text)

getListingBookPaymentSuccessR :: ListingId -> Handler TypedContent
getListingBookPaymentSuccessR lid = do
  params      <- reqGetParams <$> getRequest
  master      <- getsYesod appSettings
  connPool    <- getsYesod appSmtpPool
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
        Stripe.GetCheckoutSessionsSessionLineItemsResponseDefault err -> sendResponseStatus status503 $ toEncoding
          ("An error has ocurred with the payment processor: " <> (T.pack $ show err))
        Stripe.GetCheckoutSessionsSessionLineItemsResponseError   err -> sendResponseStatus status503 $ toEncoding
          ("An error has ocurred with the payment processor: " <> T.pack err)

      for_ items $ \i -> case Stripe.itemPrice i of
        Just (Stripe.NonNull ip) -> do
          product <- case Stripe.itemPrice'NonNullableProduct ip of
            Just (Stripe.ItemPrice'NonNullableProduct'Text           p) -> do
              productResponse <- liftIO . Stripe.runWithConfiguration conf . Stripe.getProductsId $ Stripe.mkGetProductsIdParameters p

              case responseBody productResponse of
                Stripe.GetProductsIdResponse200 products -> pure $ Stripe.productMetadata products
                Stripe.GetProductsIdResponseDefault err -> sendResponseStatus status503 $ toEncoding
                  ("An error has ocurred with the payment processor: " <> (T.pack $ show err))
                Stripe.GetProductsIdResponseError   err -> sendResponseStatus status503 $ toEncoding
                  ("An error has ocurred with the payment processor: " <> T.pack err)

            Just (Stripe.ItemPrice'NonNullableProduct'Product        p) -> pure $ Stripe.productMetadata p
            Just (Stripe.ItemPrice'NonNullableProduct'DeletedProduct p) -> sendResponseStatus status503 $ toEncoding
              ("An error has ocurred with the payment processor, the product was deleted: " <> Stripe.deletedProductId p)

          let dates = map fromJSON $ A.elems product
          case dates of
            [Success start', Success end'] -> do
              let (start, end) = if start' > end' then (end', start') else (start', end')

              for_ [start .. end] $ \d -> do
                uuid <- UUID.toText <$> liftIO randomIO
                runDB . flip upsert [EventBooked =. True] $ Event lid Local uuid
                  d end Nothing Nothing Nothing False True
              uuid  <- UUID.toText <$> liftIO randomIO
              uuid' <- UUID.toText <$> liftIO randomIO
              runDB $ do
                flip upsert [EventBlocked =. True] $ Event lid Local uuid
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
                              
                              insertUnique_ $ Checkout lid eid checkoutSessionId customerName customerEmail False
                              
                              emailBody <- defaultEmailLayout $(whamletFile "templates/email/book-confirm.hamlet")
                              liftIO $ sendEmail connPool $ (emptyMail (Address Nothing appEmail'))
                                { mailTo      = [Address Nothing customerEmail]
                                , mailHeaders = [("Subject", "Booking confirmed - " <> listingTitle listing)]
                                , mailParts   = [[htmlPart $ renderHtml emailBody]]
                                }
                            Nothing -> sendResponseStatus status500 $ toEncoding
                              ("An error has ocurred: no reservation found at " <> showGregorian start)
                        _ -> sendResponseStatus status503 $ toEncoding
                          ("An error has ocurred with the payment processor: no email was provided" :: Text)
                    _ -> sendResponseStatus status503 $ toEncoding
                      ("An error has ocurred with the payment processor: no email was provided" :: Text)
                Stripe.GetCheckoutSessionsSessionResponseDefault err -> sendResponseStatus status503 $ toEncoding
                  ("An error has ocurred with the payment processor: " <> (T.pack $ show err))
                Stripe.GetCheckoutSessionsSessionResponseError   err -> sendResponseStatus status503 $ toEncoding
                  ("An error has ocurred with the payment processor: " <> T.pack err)

              emailBody <- defaultEmailLayout $(whamletFile "templates/email/book-alert.hamlet")
              for_ adminEmails $ \adminEmail -> sendEmail connPool $ (emptyMail (Address Nothing appEmail'))
                { mailTo      = [Address Nothing adminEmail]
                , mailHeaders = [("Subject", "New booking - " <> listingTitle listing)]
                , mailParts   = [[htmlPart $ renderHtml emailBody]]
                }


      redirect ViewBookSuccessR

    (_, Nothing) -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)
    _ -> sendResponseStatus status400 $ toEncoding
      ("The payment processor did not provide a proper redirect address, missing <session_id> parameter" :: Text)

getListingBookPaymentCancelR :: ListingId -> Handler TypedContent
getListingBookPaymentCancelR lid = undefined
