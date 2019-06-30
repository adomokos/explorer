{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  -- ( run
  -- )
           where

import qualified Control.Monad.Logger as ML
import qualified Data.Pool as DP
import qualified Database.Persist.Sqlite as DB
import Entities
import qualified GitHubProxy as GP
import Import
import qualified RIO.Text as T

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

  pplCount <- runDb countPeople

  logInfo $ "Number of ppl: " <> displayShow pplCount

countPeople :: (MonadIO m) => DB.SqlPersistT m Int
countPeople = DB.count ([] :: [DB.Filter Person])

connString :: T.Text
connString = "db/explorer-db.sqlt"

runDb :: (HasConnPool env) => DB.SqlPersistT (RIO env) b -> RIO env b
  -- :: (MonadReader s m, HasConnPool s, MonadUnliftIO m)
  -- => DB.SqlPersistT m b
  -- -> m b
runDb query = do
  env <- ask
  let connPool = view connPoolL env
  DB.runSqlPool query connPool

createDbPool :: (MonadUnliftIO m) => m (DP.Pool DB.SqlBackend)
createDbPool = ML.runStdoutLoggingT $ DB.createSqlitePool connString 4
