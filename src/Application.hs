{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod
import Yesod.Auth
import Yesod.Default.Config2

import Control.Monad.Logger         (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Handler.Admin
import Handler.Admin.Listing
import Handler.Admin.Export
import Handler.Admin.Import
import Handler.Home

import Settings

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  settings <- loadYamlSettings ["config/settings.yml"] [] useEnv
  runStderrLoggingT . withSqlitePool "dev.sqlite3" 10 $ \pool -> liftIO $ do
    runResourceT . flip runSqlPool pool $ runMigration migrateAll
    warp (appPort settings) $ App settings pool
