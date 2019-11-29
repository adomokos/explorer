module Explorer.Run where

import Explorer.Import

import Data.List as L
import Data.Maybe (fromJust)
import Data.Ord ()
import qualified Data.Vector as DV
import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import Explorer.Entities
import qualified Explorer.GitHubProxy as GP
import Explorer.Util (showAndReturnEither)
import GitHub (User(..))
import qualified GitHub as GH
import qualified RIO.Text as T

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
           retrieveGitHubMetric
  fetchPerson = DB.runDb . DB.fetchPersonByGhUsername
  insertGHMetric =
      either (const $ logInfo "No GitHubMetrics was found")
             (DB.runDb . DP.insert_)

retrieveGitHubMetric
  :: MonadUnliftIO m
  => DP.Entity Person -> m (Either AppError GitHubMetric)
retrieveGitHubMetric (DP.Entity personKey person) = do
  res <- GP.fetchUserAndRepos $ T.pack $ personGitHubUsername person

  pure $ case res of
    Left _ghError -> Left $ GitHubQueryFailed "failed"
    Right result -> (Right . uncurry (buildGitHubMetric personKey)) result

buildGitHubMetric
  :: DP.Key Person
  -> User
  -> Vector GH.Repo
  -> GitHubMetric
buildGitHubMetric personKey userInfo reposData =
  GitHubMetric personKey
               (show $ userLogin userInfo)
               (T.unpack $ fromJust $ userName userInfo)
               (userPublicGists userInfo)
               (userPublicRepos userInfo)
               (userFollowers userInfo)
               (userFollowing userInfo)
               (Just . T.pack . show $ mostStarGazedRepos)
               (userCreatedAt userInfo)
 where
  mostStarGazedRepos =
    take 3 . L.sortOn (Down . snd) . DV.toList . DV.map
      (\x -> (GH.repoName x, GH.repoStargazersCount x)) $ reposData
