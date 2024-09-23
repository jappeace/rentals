module Rentals.Handler.View.Booking where

import Rentals.Foundation
import Yesod



getViewBookSuccessR :: Handler Html
getViewBookSuccessR = defaultUserLayout $ do
  $(whamletFile "templates/user/book-success.hamlet")
