{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Run
  -- ( run
  -- )
           where

import qualified Control.Monad.Logger as ML
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as DB
import qualified Entities as E
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
  peopleCount <- liftIO $ runAction countPeople
  logInfo $ displayShow peopleCount
  logInfo "Hello"

countPeople :: (MonadIO m) => DB.SqlPersistT m Int
countPeople = DB.count ([] :: [DB.Filter E.Person])

runAction :: (MonadUnliftIO m) => ReaderT DB.SqlBackend (ML.LoggingT m) b -> m b
runAction action =
  ML.runStdoutLoggingT $ DB.withSqliteConn connString $ \backend ->
    runReaderT action backend

-- runAction' action = DB.runSqlite connString action

connString :: T.Text
connString = "db/explorer-db.sqlt"

