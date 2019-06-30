{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Run
  -- ( run
  -- )
           where

import qualified Control.Monad.Logger as ML
import qualified Data.Pool as DP
import qualified Data.Text as T
-- import qualified Database.Persist.Sql (runSqlPool)
import qualified Database.Persist.Sqlite as DB
import Entities
import qualified GitHubProxy as GP
import Import

run' :: RIO App ()
run' = do
  (user, sshKeys, repo) <- GP.runAsync
    (GP.fetchUser "adomokos")
    (GP.fetchPublicSSHKeys "adomokos")
    (GP.fetchRepo "adomokos" "light-service")
    (GP.fetchRepos "adomokos")

  logInfo $ showEither user
  logInfo $ showEither sshKeys
  logInfo $ showEither repo
  -- logInfo $ showEither repos

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Error: " <> displayShow err
  Right value -> displayShow value

run :: RIO App ()
run = do
  logInfo "Hello"

  pplCount <- liftIO $ runDb countPeople

  logInfo $ "Number of ppl: " <> displayShow pplCount

countPeople :: (MonadIO m) => DB.SqlPersistT m Int
countPeople = DB.count ([] :: [DB.Filter Person])

connString :: T.Text
connString = "db/explorer-db.sqlt"

runDb :: (MonadUnliftIO m) => ReaderT DB.SqlBackend m b -> m b
runDb query = do
  pool <- createDbPool
  DB.runSqlPool query pool

createDbPool :: (MonadUnliftIO m) => m (DP.Pool DB.SqlBackend)
createDbPool = ML.runStdoutLoggingT $ DB.createSqlitePool connString 4
