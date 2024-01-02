module Rentals.Handler.View.Booking where

import Rentals.Foundation
import Yesod
import Rentals.Database.Listing
import Rentals.Database.ListingImage
import Rentals.Database.Event

import Rentals.JSON

import           Control.Monad
import           Data.Maybe
import           Text.Hamlet
import           Text.Julius

getViewBookSuccessR :: Handler Html
getViewBookSuccessR = defaultUserLayout $ do
  $(whamletFile "templates/user/book-success.hamlet")
