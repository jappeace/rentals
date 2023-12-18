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



module Rentals.Database.Listing where

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

newtype Money = Money { unMoney :: Centi }
  deriving (Eq, Ord, Num, Fractional)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Money)


instance PersistField Money where
  -- TODO: make double sure that this is precise enough, safe, to deal with money values
  toPersistValue = PersistInt64 . fromIntegral . fromEnum . unMoney
  fromPersistValue (PersistInt64 money) = Right . Money . toEnum . fromIntegral $ money
instance PersistFieldSql Money where
  sqlType _ = SqlInt64

instance ToMarkup Money where
  toMarkup = toMarkup . showFixed False . unMoney
  preEscapedToMarkup = preEscapedToMarkup . showFixed False . unMoney

newtype Slug = Slug { unSlug :: Text }
  deriving (Eq, Show, Read)
$(deriveJSON (defaultOptions {unwrapUnaryRecords = True}) ''Slug)


instance PersistField Slug where
  toPersistValue = PersistText . unSlug
  fromPersistValue (PersistText slug) = Right $ Slug slug
instance PersistFieldSql Slug where
  sqlType _ = SqlString

instance PathPiece Slug where
  toPathPiece = toPathPiece . unSlug
  fromPathPiece = fmap Slug . fromPathPiece

instance ToMarkup Slug where
  toMarkup = toMarkup . unSlug
  preEscapedToMarkup = preEscapedToMarkup . unSlug

mkPersistWith sqlSettings $(discoverEntities)
  [persistLowerCase|
Listing json
  title          Text
  description    Text
  price          Money
  slug           Slug
  uuid           UUID

  UniqueSlug slug
  UniqueCalendar uuid
  |]
