module Rentals.Handler.User.Internal where

import Yesod
import Rentals.Foundation

import           Control.Exception         (try)
import           Data.List                 (foldl')
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Calendar
import           Network.HTTP.Client       (HttpException)
import           Network.HTTP.Types.Status
import Rentals.Database.Listing
import Rentals.Database.Event
import Rentals.Database.Money
import Rentals.Settings                    (Environment(..), appEnv)
import qualified StripeAPI                 as Stripe

getQuote :: ListingId -> Day -> Day -> Handler (Money, Money)
getQuote lid start end = runDB $ do
  mlisting <- get lid

  case mlisting of
    Just listing -> do
      prices <- catMaybes . map (eventPrice . entityVal) <$> selectList
        [ EventListing ==. lid
        , EventStart >=. start
        , EventStart <=. end
        , EventPrice !=. Nothing
        ] []

      let days = length [start .. end] - length prices
      pure $ ((listingPrice listing) * (Money $ realToFrac days) + foldl' (+) 0 prices, listingCleaning listing)

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

-- | Run a Stripe API call, catching HTTP-level exceptions (DNS failure,
--   timeout, connection refused) and returning an env-aware error.
runStripe :: Stripe.Configuration -> Stripe.ClientT IO a -> Handler a
runStripe conf action = do
  result <- liftIO $ try (Stripe.runWithConfiguration conf action)
  case result of
    Right response -> pure response
    Left httpException -> do
      env <- appEnv . appSettings <$> getYesod
      $logError $ "Stripe HTTP error: " <> T.pack (show (httpException :: HttpException))
      case env of
        EnvDev  -> sendResponseStatus status503 $ toEncoding
          ("Payment processor connection error: " <> T.pack (show httpException))
        EnvProd -> sendResponseStatus status503 $ toEncoding
          ("Something went wrong" :: Text)

-- | Handle Stripe API-level errors with env-aware messaging.
stripeError :: Text -> Handler a
stripeError details = do
  env <- appEnv . appSettings <$> getYesod
  $logError $ "Stripe API error: " <> details
  case env of
    EnvDev  -> sendResponseStatus status503 $ toEncoding
      ("Payment processor error: " <> details)
    EnvProd -> sendResponseStatus status503 $ toEncoding
      ("Something went wrong" :: Text)

