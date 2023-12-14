module Handler.User.Listing where

import           Foundation
import           Yesod

import           Utils

import           Data.Fixed
import           Data.Maybe
import           Data.List (foldl')
import           Data.Text (Text)
import           Network.HTTP.Types.Status

postListingQuoteR :: Slug -> Handler TypedContent
postListingQuoteR slug = do
  (start, end) <- parseJsonBody'

  quote <- runDB $ do
    mlisting <- getBy $ UniqueSlug slug
    case mlisting of
      Just (Entity lid listing) -> do
        mcalendar <- getBy $ UniqueCalendar lid
        case mcalendar of
          Just (Entity cid _) -> do
            prices <- catMaybes . map (eventPrice . entityVal) <$> selectList
              [ EventCalendar ==. cid
              , EventStart >=. start
              , EventStart <=. end
              , EventPrice !=. Nothing
              ] []

            let days = length [start .. end] - length prices
            pure $ (listingPrice listing) * (Money $ realToFrac days) + foldl' (+) 0 prices

          Nothing -> sendResponseStatus status404 $ toEncoding
            ("The target listing does not exist, please check the identifier and try again" :: Text)
      Nothing -> sendResponseStatus status404 $ toEncoding
        ("The target listing does not exist, please check the identifier and try again" :: Text)

  sendResponseStatus status200 $ toEncoding quote
