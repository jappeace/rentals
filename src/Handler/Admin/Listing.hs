module Handler.Admin.Listing where

import Foundation
import Yesod

import Utils

import           Data.Text                 (Text)
import           Network.HTTP.Types.Status
import           System.Random

getListingR :: ListingId -> Handler Html
getListingR lid = undefined

getNewListingR :: Handler Html
getNewListingR = undefined

postNewListingR :: Handler Html
postNewListingR = do
  listing <- runInputPost $ Listing
    <$> ireq textField "title"
    <*> ireq textField "description"
    <*> (realToFrac <$> ireq doubleField "price")

  (lid, mcid) <- runDB $ do
    lid  <- insert listing
    uuid <- liftIO randomIO
    cid  <- insertUnique $ Calendar lid emptyVCalendar uuid
    pure (lid, cid)

  case mcid of
    Just cid -> do
      redirectWith status201 $ ListingR lid
    Nothing  -> do
      setMessage $ toHtml ("Failed to generate unique identifier, please try again" :: Text)
      redirectWith status500 NewListingR
