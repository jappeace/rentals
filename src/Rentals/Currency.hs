{-# LANGUAGE DeriveAnyClass #-}
module Rentals.Currency where

import Data.Text (Text)
import           Database.Persist
import           Database.Persist.Sql
import Rentals.Tshow (tshow)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Currency = Euro
              | UsDollar
              deriving stock Generic
              deriving anyclass (ToJSON, FromJSON)


instance PersistField Currency where
  toPersistValue = PersistText . toStripe
  fromPersistValue (PersistText x) = case fromStripe x of
    Just y -> pure y
    Nothing -> Left $ "unkown val" <> x
  fromPersistValue other = Left $ "unkown type " <> tshow other

instance PersistFieldSql Currency where
  sqlType _ = SqlString

printCurrency :: Currency -> Text
printCurrency = \case
  Euro -> "€"
  UsDollar -> "$"

fromStripe :: Text -> Maybe Currency
fromStripe = \case
  "EUR" -> Just Euro
  "USD" -> Just UsDollar
  _ -> Nothing

toStripe :: Currency -> Text
toStripe = \case
  Euro -> "EUR"
  UsDollar -> "USD"
