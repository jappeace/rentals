module Main where

import Test.Hspec
import BookingSpec (bookingSpec)

main :: IO ()
main = hspec bookingSpec
