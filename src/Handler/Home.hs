{-# LANGUAGE TemplateHaskell #-}
module Handler.Home where

import Foundation
import Yesod
import Yesod.Paginator

import Utils

import Text.Hamlet

getHomeR :: Handler Html
getHomeR = do
  amount <- runDB $ count [ListingActive ==. True]
  pages  <- runDB $ selectPaginated 10 [] [Asc ListingTitle]

  defaultLayout $(whamletFile "templates/home.hamlet")
