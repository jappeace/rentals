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

module Rentals.Database.Checkout where

import Rentals.Orphans()
import Rentals.Database.Event
import Data.Text
import Database.Persist.TH
import Data.UUID(UUID)
import Data.Fixed(Centi, showFixed)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import           Database.Persist
import           Database.Persist.Sql
import Text.Blaze(ToMarkup(..))
import Web.PathPieces
import Rentals.Database.Listing
import           Data.Time.Calendar
import Rentals.Database.Source


mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
Checkout
  listing   ListingId
  event     EventId
  sessionId Text
  name      Text
  email     Text

  emailed   Bool

  UniqueCheckout sessionId
  UniqueCheckoutEvent event

  |]
