module Rentals.Database.Source where

import Rentals.Database.Listing
import Rentals.Orphans()
import Data.Text
import Database.Persist.TH
import Data.UUID(UUID)
import Data.Fixed(Centi, showFixed)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import           Database.Persist
import           Database.Persist.Sql
import Text.Blaze(ToMarkup(..))
import Web.PathPieces
import           Network.URI
import           Network.URI
import qualified Data.Text                  as T
import           Control.Arrow
import           Text.Read                  (readMaybe, readEither)

data Source = Local | Airbnb | Vrbo
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Source)

instance PersistField Source where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText cs) = left T.pack . readEither $ T.unpack cs
instance PersistFieldSql Source where
  sqlType _ = SqlString

