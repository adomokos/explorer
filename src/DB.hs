{-# LANGUAGE FlexibleContexts #-}
module DB where

import Import

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import qualified Data.Time as DT
import Database.Persist.Sqlite
import Entities
import qualified RIO.Text as T

-- | Run functions from this module
run :: RIO App ()
run = do
  personId <- DB.runDb $ do
    deleteWhere ([] :: [Filter GitHubInfo])
    deleteWhere ([] :: [Filter Person])

    personId <- createPerson
    createGitHubInfo personId
    pure personId

  logInfo $ "New person Id: " <> (displayShow . fromSqlKey $ personId)

  pplCount <- DB.runDb DB.countPeople
  logInfo $ "Number of ppl: " <> displayShow pplCount

countPeople :: (MonadIO m) => SqlPersistT m Int
countPeople = count ([] :: [Filter Person])

createPerson :: (MonadIO m) => SqlPersistT m PersonId
createPerson = insert $ Person "jsmith" "John" "Smith"

createGitHubInfo :: (MonadIO m) => PersonId -> SqlPersistT m ()
createGitHubInfo personId =
  insert_ $ GitHubInfo personId "jsmith" "John Smith" $ DT.UTCTime
    (DT.fromGregorian 2009 11 2)
    (12 * 60 * 60 + 34 * 60 + 56)

connString :: T.Text
connString = "db/explorer-db.sqlt"

runDb :: SqlPersistT (RIO App) m -> RIO App m
runDb query = do
  connPool <- view $ to appConnPool
  runSqlPool query connPool

createDbPool :: (MonadUnliftIO m) => m (Pool SqlBackend)
createDbPool = runStdoutLoggingT $ createSqlitePool connString 4
