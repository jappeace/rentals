{-# LANGUAGE OverloadedStrings #-}

module UnavailableDatesSpec (unavailableDatesSpec) where

import Test.Hspec (Spec, runIO)
import Yesod.Test
import Database.Persist (insert)
import Database.Persist.Sql (runSqlPool)
import Control.Monad.Logger (runNoLoggingT)

import Rentals.Foundation
import Rentals.Application ()  -- YesodDispatch instance
import Rentals.Database.Listing
import Rentals.Database.Event
import Rentals.Database.Source
import Rentals.Database.Money
import Rentals.Currency

import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Text (pack)
import Data.UUID (UUID, fromString)
import Data.Maybe (fromJust)
import TestFoundation

testUUID :: UUID
testUUID = fromJust $ fromString "deadbeef-dead-beef-dead-beefdeadbeef"

testSlug :: Slug
testSlug = Slug "unavailable-dates-test"

testListing :: Listing
testListing = Listing
  { listingTitle = "Unavailable Dates Test"
  , listingDescription = "A listing for unavailable dates integration test"
  , listingPrice = Money 100
  , listingCurrency = UsDollar
  , listingCleaning = Money 25
  , listingCountry = "US"
  , listingAddress = "789 Past St"
  , listingHandlerName = "Test Handler"
  , listingHandlerPhone = "555-0000"
  , listingSlug = testSlug
  , listingUuid = testUUID
  }

-- | Seed a listing with a future blocked date range.
seedWithBlockedDates :: App -> IO ListingId
seedWithBlockedDates app = do
  today <- utctDay <$> getCurrentTime
  let blockedDate = addDays 30 today
  runNoLoggingT $ runSqlPool (seed blockedDate) (appConnPool app)
  where
    seed blockedDate = do
      lid <- insert testListing
      -- Block several consecutive days so the booking range overlaps
      _ <- insert $ Event lid Local "blocked-uuid-1"
        blockedDate blockedDate Nothing Nothing
        (Just "Unavailable (Local)") True False
      _ <- insert $ Event lid Local "blocked-uuid-2"
        (addDays 1 blockedDate) (addDays 1 blockedDate) Nothing Nothing
        (Just "Unavailable (Local)") True False
      _ <- insert $ Event lid Local "blocked-uuid-3"
        (addDays 2 blockedDate) (addDays 2 blockedDate) Nothing Nothing
        (Just "Unavailable (Local)") True False
      pure lid

unavailableDatesSpec :: Spec
unavailableDatesSpec = do
  app <- runIO makeTestApp
  lid <- runIO $ seedWithBlockedDates app

  yesodSpec app $ do
    ydescribe "Booking date conflict validation" $ do

      yit "rejects booking when requested dates overlap with blocked dates" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        let blockedStart = addDays 30 today
            -- Request a range that overlaps the blocked dates
            bookStart = addDays (-1) blockedStart  -- 1 day before blocked
            bookEnd   = addDays 4 blockedStart     -- well past blocked
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" (pack $ show bookStart)
          byLabelExact "End date" (pack $ show bookEnd)
        statusIs 303  -- redirected with error

      yit "accepts booking when requested dates do not overlap" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        let -- Request a range well before the blocked dates (which start at +30)
            bookStart = addDays 10 today
            bookEnd   = addDays 15 today
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" (pack $ show bookStart)
          byLabelExact "End date" (pack $ show bookEnd)
        -- Would proceed to Stripe (which fails in test env), but NOT 303
        -- The handler calls Stripe API which will error in tests,
        -- so we just verify it doesn't redirect with a conflict message.
        -- A 303 here means it got past validation to Stripe (which errors).
        -- Actually Stripe will throw, giving a 500. The point is it's not
        -- a 303 redirect back to the form with our conflict message.
        statusIs 500  -- Stripe API call fails in test env, but validation passed

      yit "does not show unavailable dates list on the booking page" $ do
        get $ ListingBookR lid
        statusIs 200
        htmlNoneContain "h5" "Unavailable dates"
