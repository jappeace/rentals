{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rentals.Settings where

import Data.Aeson
import Data.Text (Text)

data AppSettings = AppSettings
  {
    appPort       :: Int
  , appAdmin      :: [AppAdmin]
  , appDatabase   :: AppDatabase
  , appStripe     :: Stripe
  }

data AppAdmin = AppAdmin
  { adminUsername :: Text
  , adminPassword :: Text
  }

data AppDatabase = AppDatabase
  { user          :: Text
  , password      :: Text
  , host          :: Text
  , port          :: Int
  , database      :: Text
  , poolsize      :: Int
  }

data Stripe = Stripe
  { stripeSecret  :: Text
  , stripePublic  :: Text
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appPort <- o .: "port"
    appAdmin <- o .: "admin"
    appDatabase <- o .: "database"
    appStripe <- o .: "stripe"
    return AppSettings {..}

instance FromJSON AppAdmin where
  parseJSON = withObject "AppAdmin" $ \o -> do
    adminUsername <- o .: "username"
    adminPassword <- o .: "password"
    return AppAdmin {..}

instance FromJSON AppDatabase where
  parseJSON = withObject "AppDatabase" $ \o -> do
    user <- o .: "user"
    password <- o .: "password"
    host <- o .: "host"
    port <- o .: "port"
    database <- o .: "database"
    poolsize <- o .: "poolsize"
    return AppDatabase {..}

instance FromJSON Stripe where
  parseJSON = withObject "Stripe" $ \o -> do
    stripeSecret <- o .: "secret"
    stripePublic <- o .: "public"
    return Stripe {..}