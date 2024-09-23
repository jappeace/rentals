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
{-# LANGUAGE TypeOperators #-}

module Rentals.Database.Checkout where

import Rentals.Orphans()
import Rentals.Database.Event
import Data.Text
import Database.Persist.TH
import           Database.Persist.Sql
import Rentals.Database.Listing


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
