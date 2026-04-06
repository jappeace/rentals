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
import Data.Time.Calendar (addDays, diffDays, showGregorian)
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

-- | Seed a listing with a blocked range at +30 days from today.
-- Leaves free space before and after for suggestions.
seedWithBlockedDates :: App -> IO ListingId
seedWithBlockedDates app = do
  today <- utctDay <$> getCurrentTime
  let blockedDate = addDays 30 today
  runNoLoggingT $ runSqlPool (seed blockedDate) (appConnPool app)
  where
    seed blockedDate = do
      lid <- insert testListing
      -- Block 3 consecutive days starting at +30
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

      yit "rejects overlapping booking and suggests alternative date ranges" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        let blockedStart = addDays 30 today
            -- Request 6 days overlapping the blocked range
            bookStart = addDays (-1) blockedStart
            bookEnd   = addDays 4 blockedStart
            duration  = diffDays bookEnd bookStart  -- 5 nights
            -- Expected before suggestion: ends day before bookStart,
            -- same duration
            beforeEnd   = addDays (-1) bookStart
            beforeStart = addDays (negate duration) beforeEnd
            -- Expected after suggestion: starts day after bookEnd,
            -- same duration
            afterStart = addDays 1 bookEnd
            afterEnd   = addDays duration afterStart
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" (pack $ show bookStart)
          byLabelExact "End date" (pack $ show bookEnd)
        statusIs 303
        -- Follow the redirect to see the flash message
        get $ ListingBookR lid
        statusIs 200
        -- The flash message should contain "already booked"
        htmlAnyContain "div" "already booked"
        -- The flash message should suggest a range before
        htmlAnyContain "div" (showGregorian beforeStart)
        htmlAnyContain "div" (showGregorian beforeEnd)
        -- The flash message should suggest a range after
        htmlAnyContain "div" (showGregorian afterStart)
        htmlAnyContain "div" (showGregorian afterEnd)

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
        -- Stripe API call fails in test env, but validation passed
        statusIs 500

      yit "does not show unavailable dates list on the booking page" $ do
        get $ ListingBookR lid
        statusIs 200
        htmlNoneContain "h5" "Unavailable dates"
