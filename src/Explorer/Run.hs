module Explorer.Run where

import Explorer.Import

import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import qualified Explorer.GitHubService as GHS
import Explorer.Util (showAndReturnEither)

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
  fetchPerson = DB.runDb . DB.fetchPersonByGhUsername
  insertGHMetric =
      either (const $ logInfo "No GitHubMetrics was found")
             (DB.runDb . DP.insert_)
