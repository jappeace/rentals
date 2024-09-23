module Rentals.Database.Source where

import Rentals.Orphans()
import Rentals.Tshow
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import           Database.Persist
import           Database.Persist.Sql
import qualified Data.Text                  as T
import           Control.Arrow
import           Text.Read                  (readEither)

data Source = Local | Airbnb | Vrbo
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Source)

instance PersistField Source where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText cs) = left T.pack . readEither $ T.unpack cs
  fromPersistValue other = Left $ "unkown val" <> tshow other

instance PersistFieldSql Source where
  sqlType _ = SqlString

