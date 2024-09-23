{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rentals.Database.Money where

import Rentals.Orphans()
import Data.Fixed(Centi, showFixed)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import           Database.Persist
import           Database.Persist.Sql
import Text.Blaze(ToMarkup(..))
import Rentals.Tshow (tshow)

newtype Money = Money { unMoney :: Centi }
  deriving (Eq, Ord, Num, Fractional)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Money)

instance PersistField Money where
  -- TODO: make double sure that this is precise enough, safe, to deal with money values
  toPersistValue = PersistInt64 . fromIntegral . fromEnum . unMoney
  fromPersistValue (PersistInt64 money) = Right . Money . toEnum . fromIntegral $ money
  fromPersistValue other = Left $ "unkown val" <> tshow other
instance PersistFieldSql Money where
  sqlType _ = SqlInt64

instance ToMarkup Money where
  toMarkup = toMarkup . showFixed False . unMoney
  preEscapedToMarkup = preEscapedToMarkup . showFixed False . unMoney

