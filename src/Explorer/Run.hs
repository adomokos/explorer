module Explorer.Run where

import Explorer.Import

import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import qualified Explorer.GitHubService as GHS
import Explorer.Util (showAndReturnEither)
import LoadEnv (loadEnv)
import RIO.Process (mkDefaultProcessContext)

run :: RIO App ()
run =
  fetchPerson ghUsername
    >>= findGHMetric
    >>= showAndReturnEither
    >>= insertGHMetric
    >> DB.showGitHubMetricCount
 where
  ghUsername = "adomokos"
  findGHMetric =
    either (pure . Left)
           GHS.retrieveGitHubMetric
  fetchPerson = DB.runDB . DB.fetchPersonByGhUsername
  insertGHMetric =
      either (const $ logInfo "No GitHubMetrics was found")
             (DB.runDB . DP.insert_)

-- | Runs a RIO App
-- For example runRIOApp $ runDB countPeople :: IO Int
runRIOApp :: RIO App a -> IO a
runRIOApp f = do
  app <- getApp
  runRIO app f

getApp :: IO App
getApp = do
  dbPool <- DB.createDbPool
  pc <- mkDefaultProcessContext

  loadEnv

  pure App
    { appLogFunc = mempty
    , appProcessContext = pc
    , appOptions = Options { optionsVerbose = False }
    , appConnPool = dbPool
    }
