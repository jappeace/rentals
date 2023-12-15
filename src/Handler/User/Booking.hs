module Handler.User.Booking where

import Yesod
import Foundation

import Handler.User.Internal

import Settings
import Utils

import           Control.Monad
import           Data.Text                                   (Text)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import qualified StripeAPI                                   as Stripe
import qualified StripeAPI.Types.NotificationEventData.Extra as Stripe

putListingBookR :: ListingId -> Handler TypedContent
putListingBookR lid = do
  (start, end) <- parseJsonBody'

  when (length [start .. end] < 3) . sendResponseStatus status400 $ toEncoding
    ("The minimum amount of days for booking is 3." :: Text)

  amount     <- (* 100) . floor . unMoney <$> getQuote lid start end
  stripeKeys <- getsYesod $ appStripe . appSettings

  let conf = Stripe.defaultConfiguration
        { Stripe.configSecurityScheme = Stripe.basicAuthenticationSecurityScheme Stripe.BasicAuthenticationData
          { Stripe.basicAuthenticationDataUsername = stripeSecret stripeKeys
          , Stripe.basicAuthenticationDataPassword = ""
          }
        }
      pirb = Stripe.mkPostPaymentIntentsRequestBody amount "USD"

  res <- liftIO . Stripe.runWithConfiguration conf $ Stripe.postPaymentIntents pirb
  case responseBody res of
    Stripe.PostPaymentIntentsResponse200 pi ->
      sendResponseStatus status204 ()

