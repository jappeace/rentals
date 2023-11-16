module Utils where

import Yesod
import Foundation

import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import           Data.Default         (def)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Version
import           Text.ICalendar

parseCalendar :: LBS.ByteString -> Handler (Either Text VCalendar)
parseCalendar c = do
  -- TODO: what is the FilePath in the parsing function exactly for?
  pure . either (Left . T.pack) (Right . head . fst) . parseICalendar def "." $ c

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
