module Main where

import Test.Hspec
import BookingSpec (bookingSpec)
import EmailSpec (emailSpec)

main :: IO ()
main = hspec $ do
  bookingSpec
  emailSpec
