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

module Rentals.Database.Event where

import Rentals.Orphans()
import Rentals.Database.Money
import Data.Text
import Database.Persist.TH
import           Database.Persist.Sql
import Rentals.Database.Listing
import           Data.Time.Calendar
import Rentals.Database.Source


-- TODO add currency a booking was made in
mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
Event
  -- app details
  listing        ListingId
  source         Source
  uuid           Text

  -- booking details
  start          Day
  end            Day
  price          Money Maybe
  description    Text Maybe

  -- meta details
  summary        Text Maybe         -- meta description of the event entry
  blocked        Bool default=false -- manually blocked?
  booked         Bool default=false

  UniqueEvent listing start

  |]
