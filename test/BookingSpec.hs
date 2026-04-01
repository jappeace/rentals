{-# LANGUAGE OverloadedStrings #-}

module BookingSpec (bookingSpec) where

import Test.Hspec (Spec, runIO)
import Yesod.Test
import Database.Persist (insert)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import Control.Monad.Logger (runNoLoggingT)

import Rentals.Foundation
import Rentals.Application ()  -- YesodDispatch instance
import Rentals.Database.Listing
import Rentals.Database.Money
import Rentals.Currency

import Data.UUID (UUID, fromString)
import Data.Maybe (fromJust)
import TestFoundation

testUUID :: UUID
testUUID = fromJust $ fromString "12345678-1234-1234-1234-123456789abc"

testSlug :: Slug
testSlug = Slug "test-listing"

testListing :: Listing
testListing = Listing
  { listingTitle = "Test Listing"
  , listingDescription = "A test listing for booking"
  , listingPrice = Money 100
  , listingCurrency = UsDollar
  , listingCleaning = Money 25
  , listingCountry = "US"
  , listingAddress = "123 Test St"
  , listingHandlerName = "Test Handler"
  , listingHandlerPhone = "555-1234"
  , listingSlug = testSlug
  , listingUuid = testUUID
  }

seedListing :: App -> IO ListingId
seedListing app =
  runNoLoggingT $ runSqlPool (insert testListing) (appConnPool app)

bookingSpec :: Spec
bookingSpec = do
  app <- runIO makeTestApp
  lid <- runIO $ seedListing app

  yesodSpec app $ do
    ydescribe "GET booking form" $ do

      yit "renders for a valid listing" $ do
        get $ ListingBookR lid
        statusIs 200
        htmlAnyContain "button" "Book now"

      yit "returns 404 for a non-existent listing" $ do
        get $ ListingBookR (toSqlKey 999)
        statusIs 404

    ydescribe "POST booking form" $ do

      yit "redirects with error when form fields are missing" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
        statusIs 303

      yit "redirects with error when start date is after end date" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" "2025-01-10"
          byLabelExact "End date" "2025-01-05"
        statusIs 303

      yit "redirects with error when booking is less than 3 days" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" "2025-01-01"
          byLabelExact "End date" "2025-01-02"
        statusIs 303

    ydescribe "Cancel handler" $ do

      yit "redirects to the booking page" $ do
        get $ ListingBookPaymentCancelR lid
        statusIs 303

    ydescribe "Listing view page" $ do

      yit "has a Book Now link to the booking page" $ do
        get $ ViewListingR lid testSlug
        statusIs 200
        htmlAnyContain "a" "Book now"
