module Rentals.Currency where

import Data.Text (Text)

data Currency = Euro
              | UsDollar

instance PersisrField Currency where

printCurrency :: Currency -> Text
printCurrency = \case
  Euro -> "€"
  UsDollar -> "$"


toStripe :: Currency -> Text
toStripe = \case
  Euro -> "EUR"
  UsDollar -> "USD"
