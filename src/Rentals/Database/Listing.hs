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
import Rentals.Database.Money
import Yesod.Core.Content
import Data.Text
import Database.Persist.TH
import Data.UUID(UUID)
import Data.Aeson(toJSON)
import Data.Fixed(Centi, showFixed)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, unwrapUnaryRecords)
import           Database.Persist
import           Database.Persist.Sql
import Text.Blaze(ToMarkup(..))
import Web.PathPieces



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
  title            Text
  description      Text
  price            Money
  cleaning         Money default=0
  country          Text
  address          Text
  handlerName      Text
  handlerPhone     Text

  slug             Slug
  uuid             UUID

  UniqueSlug slug
  UniqueCalendar uuid
  |]


instance ToContent Listing where
  toContent = toContent . toJSON
instance ToTypedContent Listing where
  toTypedContent = TypedContent "application/json; charset=utf-8" . toContent
