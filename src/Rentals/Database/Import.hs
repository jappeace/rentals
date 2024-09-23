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

module Rentals.Database.Import where

import Rentals.Database.Listing
import Rentals.Orphans()
import Database.Persist.TH
import           Database.Persist.Sql
import           Network.URI
import Rentals.Database.Source

mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
Import
  listing        ListingId
  source         Source
  uri            URI

  UniqueImport listing source
  |]
