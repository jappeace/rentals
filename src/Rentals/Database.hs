{-# LANGUAGE TemplateHaskell #-}

-- | The database schema definition of the app.
--   this prescriptevaly tells how the app expect the database to look
module Rentals.Database(migrateAll) where

import Database.Persist.TH
import Database.Persist.Sql (Migration)
import Rentals.Database.Listing()
import Rentals.Database.ListingImage()
import Rentals.Database.Event()
import Rentals.Database.Checkout()
import Rentals.Database.Import()
import Rentals.Database.Source()
import Rentals.Database.Money()

-- this module splitting trick is mentioned here https://www.parsonsmatt.org/2019/12/06/splitting_persistent_models.html
-- but if we use discoverEntities then we have to use it in all of our models, otherwise foreign keys are missed
-- e.g. see https://github.com/yesodweb/persistent/issues/1307
migrateAll :: Migration
migrateAll = migrateModels $(discoverEntities)
