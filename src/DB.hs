module DB where

import Import

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sqlite
  (Filter, SqlBackend, SqlPersistT, count, createSqlitePool, runSqlPool)
import Entities
import qualified RIO.Text as T

countPeople :: (MonadIO m) => SqlPersistT m Int
countPeople = count ([] :: [Filter Person])

connString :: T.Text
connString = "db/explorer-db.sqlt"

-- :: (MonadReader s m, HasConnPool s, MonadUnliftIO m)
-- => DB.SqlPersistT m b
-- -> m b
runDb :: (HasConnPool env) => SqlPersistT (RIO env) b -> RIO env b
runDb query = do
  env <- ask
  let connPool = view connPoolL env
  runSqlPool query connPool

createDbPool :: (MonadUnliftIO m) => m (Pool SqlBackend)
createDbPool = runStdoutLoggingT $ createSqlitePool connString 4
