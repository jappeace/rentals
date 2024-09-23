module Rentals.Calendar(addVEventToVCalendar, emptyVCalendar, parseCalendar, printCalendar, newVEvent) where

import Rentals.Foundation

import qualified Data.ByteString.Lazy      as LBS
import           Data.CaseInsensitive
import           Data.Default              (def)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Time.Clock
import           Data.UUID
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

newVEvent :: UTCTime -> UUID -> VEvent
newVEvent currentTime uuid = VEvent
  { veDTStamp       = Just $ DTStamp currentTime def
  , veUID           = UID (LT.fromStrict . toText $ uuid) def
  , veClass         = Class Private def
  , veDTStart       = Nothing
  , veCreated       = Nothing
  , veDescription   = Nothing
  , veGeo           = Nothing
  , veLastMod       = Nothing
  , veLocation      = Nothing
  , veOrganizer     = Nothing
  , vePriority      = Priority 0 def
  , veSeq           = Sequence 0 def
  , veStatus        = Nothing
  , veSummary       = Nothing
  , veTransp        = Opaque def
  , veUrl           = Nothing
  , veRecurId       = Nothing
  , veRRule         = mempty
  , veDTEndDuration = Nothing
  , veAttach        = mempty
  , veAttendee      = mempty
  , veCategories    = mempty
  , veComment       = mempty
  , veContact       = mempty
  , veExDate        = mempty
  , veRStatus       = mempty
  , veRelated       = mempty
  , veResources     = mempty
  , veRDate         = mempty
  , veAlarms        = mempty
  , veOther         = mempty
  }

addVEventToVCalendar :: VCalendar -> VEvent -> VCalendar
addVEventToVCalendar cal evn
  | M.member (uidValue (veUID evn), Nothing) (vcEvents cal) = cal
  | otherwise = cal {vcEvents = M.insert (uidValue (veUID evn), Nothing) evn (vcEvents cal)}
