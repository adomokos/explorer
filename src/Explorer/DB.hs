module Explorer.DB where

import Explorer.Import

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import qualified Data.Time as DT
import Database.Persist.Sqlite
import Explorer.Entities
import Explorer.Util (toEither)
import RIO.Partial (fromJust)
import qualified RIO.Text as T
import System.Environment (lookupEnv)

-- | Run functions from this module
run :: RIO App ()
run = do
  personId <- runDB $ do
    deleteWhere ([] :: [Filter GitHubMetric])
    Entity personId _ <- getJustEntity (PersonKey 1)

    createGitHubMetric personId
    pure personId

  logInfo $ "New person Id: " <> (displayShow . fromSqlKey $ personId)

  pplCount <- runDB countPeople
  logInfo $ "Number of ppl: " <> displayShow pplCount

countPeople :: (MonadIO m) => SqlPersistT m Int
countPeople = count ([] :: [Filter Person])

countGHMetrics :: (MonadIO m) => SqlPersistT m Int
countGHMetrics = count ([] :: [Filter GitHubMetric])

createPerson :: (MonadIO m) => SqlPersistT m PersonId
createPerson = insert $ Person "jsmith" "John" "Smith" "adomokos"

fetchPersonByGhUsername
  :: (MonadIO m)
  => Text
  -> SqlPersistT m (Either AppError (Entity Person))
fetchPersonByGhUsername gitHubUsername =
  toEither (UserNotFound gitHubUsername) <$>
    selectFirst [PersonGitHubUsername ==. T.unpack gitHubUsername] []

createGitHubMetric :: (MonadIO m) => PersonId -> SqlPersistT m ()
createGitHubMetric personId =
  insert_ $ GitHubMetric personId "jsmith" "John Smith" 34 79 100 9 Nothing dummyDate

showGitHubMetricCount :: RIO App ()
showGitHubMetricCount =
  runDB $ do
    ghMetricCount <- countGHMetrics

    lift . logInfo $ "Number of ghMetrics: " <> displayShow ghMetricCount

dummyDate :: DT.UTCTime
dummyDate =
  DT.UTCTime (DT.fromGregorian 2009 11 2) (12 * 60 * 60 + 34 * 60 + 56)

connString :: IO T.Text
connString = T.pack . fromJust <$> lookupEnv "DB_FILE_PATH"

-- | Runs a DB action, but 'runApp' is needed to get the RIO App context
runDB :: (MonadUnliftIO a, MonadReader App a) => SqlPersistT a m -> a m
runDB query = do
  connPool <- view $ to appConnPool
  runSqlPool query connPool

createDbPool :: (MonadUnliftIO m) => m (Pool SqlBackend)
createDbPool = do
  dbConnString <- liftIO connString
  runStdoutLoggingT $ createSqlitePool dbConnString 4
