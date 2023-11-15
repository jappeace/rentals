module Utils where

import Yesod
import Foundation

import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import           Data.Default         (def)
import           Data.Version
import           Text.ICalendar

parseCalendar :: LBS.ByteString -> Handler VCalendar
parseCalendar c = do
  case parseICalendar def "." c of
    Right (pc:_, _) -> return pc
    Left err -> undefined

printCalendar :: VCalendar -> Handler LBS.ByteString
printCalendar c = do
  return . printICalendar def $ c

emptyVCalendar :: VCalendar
emptyVCalendar = VCalendar
  { vcProdId     = ProdId "rentals-0.0.0" def
  , vcVersion    = MaxICalVersion (makeVersion [2, 0]) def
  , vcScale      = Scale (mk "GREGORIAN") def
  , vcMethod     = Nothing
  , vcOther      = mempty
  , vcTimeZones  = mempty
  , vcEvents     = mempty
  , vcTodos      = mempty
  , vcJournals   = mempty
  , vcFreeBusys  = mempty
  , vcOtherComps = mempty
  }
