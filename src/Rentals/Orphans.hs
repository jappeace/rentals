{-# OPTIONS_GHC -Wno-orphans #-}
-- | following the ideas described in this blogpost:
--  https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/
--
--  we group all orphans (we can) in here.
--  this excludes some nefarious yesod based orphans.
module Rentals.Orphans where

import Rentals.Tshow
import           Data.Time.Calendar
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Database.Persist
import           Database.Persist.Sql
import     Web.PathPieces
import           Network.URI
import qualified Data.Text                  as T
import Text.Blaze(ToMarkup(..))
import Text.ICalendar (VCalendar)
import Yesod.Core.Content (ToContent(..))
import Yesod.Core (ToTypedContent(..))
import Text.ICalendar.Printer
import Yesod.Core (TypedContent(..))
import Data.Default (def)

instance PersistField UUID where
  toPersistValue = PersistText . UUID.toText
  fromPersistValue (PersistText uuid) =
    case UUID.fromText uuid of
      Just uuid' -> Right uuid'
      Nothing -> Left "Failed to create UUID from Text"
  fromPersistValue other = Left $ "unkown val" <> tshow other

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PathPiece UUID where
  toPathPiece uuid = UUID.toText uuid
  fromPathPiece p = UUID.fromText p


instance PersistField URI where
  toPersistValue uri = PersistText . T.pack $ (uriToString id uri) ""
  fromPersistValue (PersistText uri) =
    case parseURI . T.unpack $ uri of
      Just uri' -> Right uri'
      Nothing -> Left "Failed to create URI from Text"
  fromPersistValue other = Left $ "unkown val" <> tshow other
instance PersistFieldSql URI where
  sqlType _ = SqlString

instance ToMarkup URI where
  toMarkup v = toMarkup $ (uriToString id v) ""
  preEscapedToMarkup v = preEscapedToMarkup $ (uriToString id v) ""

instance ToMarkup Day where
  toMarkup = toMarkup . showGregorian
  preEscapedToMarkup = preEscapedToMarkup . showGregorian

instance ToContent VCalendar where
  toContent = toContent . printICalendar def
instance ToTypedContent VCalendar where
  toTypedContent = TypedContent "text/calendar; charset=utf-8" . toContent

instance ToMarkup UUID where
  toMarkup = toMarkup . UUID.toText
  preEscapedToMarkup = preEscapedToMarkup . UUID.toText
