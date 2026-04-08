{-# LANGUAGE OverloadedStrings #-}

module BookingSpec (bookingSpec) where

import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Yesod.Test
import Database.Persist (insert)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import Control.Monad.Logger (runNoLoggingT)

import Rentals.Foundation
import Rentals.Application ()  -- YesodDispatch instance
import Rentals.Database.Listing
import Rentals.Database.Money
import Rentals.Handler.User.Internal (calculateNightlyCharge)
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
  , listingPricePerExtraPerson = Money 0
  , listingMaxPeople = 4
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
          byLabelExact "Number of guests" "1"
        statusIs 303

      yit "redirects with error when booking is less than 3 days" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" "2025-01-01"
          byLabelExact "End date" "2025-01-02"
          byLabelExact "Number of guests" "1"
        statusIs 303

    ydescribe "Cancel handler" $ do

      yit "redirects to the booking page" $ do
        get $ ListingBookPaymentCancelR lid
        statusIs 303

    ydescribe "Guest count validation" $ do

      yit "redirects when guest count exceeds max people" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" "2025-06-01"
          byLabelExact "End date" "2025-06-05"
          byLabelExact "Number of guests" "5"
        statusIs 303

      yit "redirects when guest count is zero" $ do
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" "2025-06-01"
          byLabelExact "End date" "2025-06-05"
          byLabelExact "Number of guests" "0"
        statusIs 303

    ydescribe "Listing view page" $ do

      yit "has a Book Now link to the booking page" $ do
        get $ ViewListingR lid testSlug
        statusIs 200
        htmlAnyContain "a" "Book now"

  describe "calculateNightlyCharge" $ do

    it "charges base price only for a single guest" $ do
      calculateNightlyCharge (Money 100) [] (Money 10) 5 1
        `shouldBe` Money 500

    it "adds extra person surcharge for multiple guests" $ do
      calculateNightlyCharge (Money 100) [] (Money 10) 5 3
        `shouldBe` Money 600

    it "combines override prices with extra person surcharge" $ do
      calculateNightlyCharge (Money 100) [Money 80, Money 90] (Money 10) 5 2
        `shouldBe` Money 520

    it "charges no surcharge when pricePerExtraPerson is zero" $ do
      calculateNightlyCharge (Money 100) [] (Money 0) 5 4
        `shouldBe` Money 500
