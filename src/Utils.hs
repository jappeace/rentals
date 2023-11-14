module Utils where

import Yesod
import Foundation

import qualified Data.ByteString.Lazy as LBS
import           Data.Default         (def)
import           Text.ICalendar

parseCalendar :: LBS.ByteString -> Handler VCalendar
parseCalendar c = do
  case parseICalendar def "." c of
    Right (pc:_, _) -> return pc
    Left err -> undefined

printCalendar :: VCalendar -> Handler LBS.ByteString
printCalendar c = do
  return . printICalendar def $ c
