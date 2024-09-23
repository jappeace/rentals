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

module Rentals.Database.ListingImage where

import Rentals.Orphans()
import Rentals.Database.Listing
import Database.Persist.TH
import Data.UUID(UUID)
import           Database.Persist.Sql


mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
ListingImage
  listing        ListingId
  uuid           UUID

  UniqueImage uuid
  |]
