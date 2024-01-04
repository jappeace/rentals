{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rentals.Settings where

import Data.Aeson
import Data.Text (Text)
import Network.Mail.Pool

data AppSettings = AppSettings
  { appPort       :: Int
  , appEmail      :: Text
  , appAdmin      :: [AppAdmin]
  , appDatabase   :: AppDatabase
  , appSmtpCreds  :: SmtpCred
  , appStripe     :: Stripe
  }

data AppAdmin = AppAdmin
  { adminUsername :: Text
  , adminPassword :: Text
  , adminEmail    :: Text
  }

data AppDatabase = AppDatabase
  { user          :: Text
  , password      :: Text
  , host          :: Text
  , port          :: Int
  , database      :: Text
  , poolsize      :: Int
  , migrationFolder :: MigrationFolder
  }

newtype MigrationFolder = MigrationFolder Text

data Stripe = Stripe
  { stripeSecret  :: Text
  , stripePublic  :: Text
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appPort <- o .: "port"
    appEmail <- o .: "email"
    appAdmin <- o .: "admin"
    appDatabase <- o .: "database"
    appSmtpCreds <- o .: "smtp"
    appStripe <- o .: "stripe"
    return AppSettings {..}

instance FromJSON AppAdmin where
  parseJSON = withObject "AppAdmin" $ \o -> do
    adminUsername <- o .: "username"
    adminPassword <- o .: "password"
    adminEmail <- o .: "email"
    return AppAdmin {..}

instance FromJSON AppDatabase where
  parseJSON = withObject "AppDatabase" $ \o -> do
    user <- o .: "user"
    password <- o .: "password"
    host <- o .: "host"
    port <- o .: "port"
    database <- o .: "database"
    poolsize <- o .: "poolsize"
    migrationFolder  <- fmap MigrationFolder $  o .: "migration_folder"
    return AppDatabase {..}

instance FromJSON Stripe where
  parseJSON = withObject "Stripe" $ \o -> do
    stripeSecret <- o .: "secret"
    stripePublic <- o .: "public"
    return Stripe {..}
