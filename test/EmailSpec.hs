{-# LANGUAGE OverloadedStrings #-}

module EmailSpec (emailSpec) where

import Test.Hspec (Spec, runIO)
import Yesod.Test
import Yesod.Auth (Route(..))
import Database.Persist (insert)
import Database.Persist.Sql (runSqlPool)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (encode)

import Rentals.Foundation
import Rentals.Application ()  -- YesodDispatch instance
import Rentals.Database.Listing
import Rentals.Database.Event
import Rentals.Database.Checkout
import Rentals.Database.Source
import Rentals.Database.Money
import Rentals.Currency

import Data.Time.Calendar
import Data.UUID (UUID, fromString)
import Data.Maybe (fromJust)
import TestFoundation

testUUID :: UUID
testUUID = fromJust $ fromString "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

testSlug :: Slug
testSlug = Slug "email-test-listing"

testListing :: Listing
testListing = Listing
  { listingTitle = "Email Test Listing"
  , listingDescription = "A listing for email integration test"
  , listingPrice = Money 100
  , listingCurrency = UsDollar
  , listingCleaning = Money 25
  , listingCountry = "US"
  , listingAddress = "456 Email St"
  , listingHandlerName = "Email Handler"
  , listingHandlerPhone = "555-5678"
  , listingSlug = testSlug
  , listingUuid = testUUID
  }

-- | Seed a listing, event, and checkout record for email testing
seedEmailTestData :: App -> IO (ListingId, CheckoutId)
seedEmailTestData app = runNoLoggingT $ runSqlPool action (appConnPool app)
  where
    action = do
      lid <- insert testListing
      eid <- insert $ Event lid Local "test-event-uuid"
        (fromGregorian 2025 6 1)
        (fromGregorian 2025 6 5)
        Nothing Nothing Nothing False True
      cid <- insert $ Checkout lid eid "cs_test_session_123" "Test Customer" "customer@example.com" False
      pure (lid, cid)

-- | Log in as admin via the hardcoded auth plugin.
--   Posts directly to the plugin route with form params.
--   No CSRF token needed: the login.hamlet is a raw HTML form
--   (not a Yesod form), so no _token is stored in the session.
loginAdmin :: YesodExample App ()
loginAdmin = do
  request $ do
    setMethod "POST"
    setUrl $ AuthR $ PluginR "hardcoded" ["login"]
    addPostParam "username" "admin"
    addPostParam "password" "admin"

emailSpec :: Spec
emailSpec = do
  app <- runIO makeTestApp
  (lid, cid) <- runIO $ seedEmailTestData app

  yesodSpec app $ do
    ydescribe "Admin email sending" $ do

      yit "sends email without SMTP connection failure" $ do
        loginAdmin
        request $ do
          setMethod "PUT"
          setUrl $ AdminListingEmailsR lid
          addRequestHeader ("Content-Type", "application/json")
          setRequestBody $ encode (cid, "Your booking instructions here" :: String)
        -- On master this would be 500 (HostCannotConnect from SMTP pool).
        -- With dev email logging it succeeds with 204.
        statusIs 204
