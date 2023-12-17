module Rentals.Handler.User.Internal where

import Yesod
import Rentals.Foundation

import           Data.List                 (foldl')
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Time.Calendar
import           Network.HTTP.Types.Status

getQuote :: ListingId -> Day -> Day -> Handler Money
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
      pure $ (listingPrice listing) * (Money $ realToFrac days) + foldl' (+) 0 prices

    Nothing -> sendResponseStatus status404 $ toEncoding
      ("The target listing does not exist, please check the identifier and try again" :: Text)

