-- | following the ideas described in this blogpost:
--  https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/
--
--  we group all orphans in here.
module Rentals.Orphans where
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           Database.Persist
import           Database.Persist.Sql
import     Web.PathPieces

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
