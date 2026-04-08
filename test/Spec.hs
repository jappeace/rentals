module Main where

import Test.Hspec
import BookingSpec (bookingSpec)
import EmailSpec (emailSpec)
import UnavailableDatesSpec (unavailableDatesSpec)
import GuestPricingSpec (guestPricingSpec)

main :: IO ()
main = hspec $ do
  bookingSpec
  emailSpec
  unavailableDatesSpec
  guestPricingSpec
