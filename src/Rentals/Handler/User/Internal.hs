module Rentals.Handler.User.Internal
  ( getQuote
  , calculateNightlyCharge
  ) where

import Yesod
import Rentals.Foundation

import           Data.List                 (foldl')
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Time.Calendar
import           Network.HTTP.Types.Status
import Rentals.Database.Listing
import Rentals.Database.Event
import Rentals.Database.Money

-- | Pure pricing calculation.
--   Extra-person surcharge applies uniformly to all nights
--   (both standard-priced and override-priced).
calculateNightlyCharge :: Money -> [Money] -> Money -> Int -> Int -> Money
calculateNightlyCharge basePrice overridePrices pricePerExtraPerson totalNights guestCount =
  let standardDays = totalNights - length overridePrices
      baseCharge   = basePrice * (Money $ realToFrac standardDays) + foldl' (+) 0 overridePrices
      extraPersons = max 0 (guestCount - 1)
      extraCharge  = pricePerExtraPerson
                   * (Money $ realToFrac extraPersons)
                   * (Money $ realToFrac totalNights)
  in baseCharge + extraCharge

getQuote :: ListingId -> Day -> Day -> Int -> Handler (Money, Money)
getQuote lid start end guestCount = runDB $ do
  mlisting <- get lid

  case mlisting of
    Just listing -> do
      overridePrices <- catMaybes . map (eventPrice . entityVal) <$> selectList
        [ EventListing ==. lid
        , EventStart >=. start
        , EventStart <=. end
        , EventPrice !=. Nothing
        ] []

      let totalNights = length [start .. end]
          nightlyCharge = calculateNightlyCharge
            (listingPrice listing)
            overridePrices
            (listingPricePerExtraPerson listing)
            totalNights
            guestCount
      pure (nightlyCharge, listingCleaning listing)

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)
