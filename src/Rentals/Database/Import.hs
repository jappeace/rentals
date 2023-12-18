{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rentals.Database.Import where

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
import Rentals.Database.Source

mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
Import
  listing        ListingId
  source         Source
  uri            URI

  UniqueImport listing source
  |]
