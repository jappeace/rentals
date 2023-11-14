{-# LANGUAGE TemplateHaskell #-}
module Handler.Home where

import Foundation
import Yesod

import Utils

import Text.Hamlet

getHomeR :: Handler Html
getHomeR = do
  listings <- runDB $ selectList [] [Asc ListingTitle]

  defaultLayout $(whamletFile "templates/home.hamlet")
