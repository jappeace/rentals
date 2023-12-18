-- | following the ideas described in this blogpost:
--  https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/
--
--  we group all orphans (we can) in here.
--  this excludes some nefarious yesod based orphans.
module Rentals.Orphans where
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Database.Persist
import           Database.Persist.Sql
import     Web.PathPieces
import           Network.URI
import qualified Data.Text                  as T
import Text.Blaze(ToMarkup(..))

instance PersistField UUID where
  toPersistValue = PersistText . UUID.toText
  fromPersistValue (PersistText uuid) =
    case UUID.fromText uuid of
      Just uuid' -> Right uuid'
      Nothing -> Left "Failed to create UUID from Text"
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
instance PersistFieldSql URI where
  sqlType _ = SqlString

instance ToMarkup URI where
  toMarkup v = toMarkup $ (uriToString id v) ""
  preEscapedToMarkup v = preEscapedToMarkup $ (uriToString id v) ""
