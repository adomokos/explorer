{-# LANGUAGE FlexibleContexts #-}
module DB where

import Import

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sqlite
  (Filter, SqlBackend, SqlPersistT, count, createSqlitePool, runSqlPool)
import Entities
import qualified RIO.Text as T

-- | Run functions from this module
run :: RIO App ()
run = do
  logInfo "Hello"

  pplCount <- DB.runDb DB.countPeople

  logInfo $ "Number of ppl: " <> displayShow pplCount

countPeople :: (MonadIO m) => SqlPersistT m Int
countPeople = count ([] :: [Filter Person])

connString :: T.Text
connString = "db/explorer-db.sqlt"

runDb :: SqlPersistT (RIO App) m -> RIO App m
runDb query = do
  connPool <- view $ to appConnPool
  runSqlPool query connPool

createDbPool :: (MonadUnliftIO m) => m (Pool SqlBackend)
createDbPool = runStdoutLoggingT $ createSqlitePool connString 4
