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
import Explorer.Util (showEither)
import GitHub (User(..))
import qualified GitHub as GH
import qualified RIO.Text as T

run :: RIO App ()
run = do
  let ghUsername = "adomokos"

  -- logInfo $ displayShow gitHubMetric

  ghMetric <- fetchPerson ghUsername
    >>= findGHMetric ghUsername

  logInfo $ showEither ghMetric

  DB.runDb $ do
    insertGHMetric ghMetric
    ghMetricCount <- DB.countGHMetrics

    lift . logInfo $ "Number of ghMetrics: " <> displayShow ghMetricCount

 where
  findGHMetric ghUsername =
    either (pure . Left)
           (retrieveGitHubMetric ghUsername)
  fetchPerson = DB.runDb . DB.fetchPersonByGhUsername'
  insertGHMetric =
    either (\_ -> lift . logInfo $ "Not GitHubMetrics was found")
           DP.insert_

retrieveGitHubMetric
  :: MonadUnliftIO m
  => Text -> DP.Entity Person -> m (Either AppError GitHubMetric)
retrieveGitHubMetric ghUsername person = do
  res <- GP.fetchUserAndRepos ghUsername

  pure $ case res of
    Left _ghError -> Left $ GitHubQueryFailed "failed"
    Right result -> (Right . uncurry (buildGitHubMetric person)) result

buildGitHubMetric
  :: DP.Entity Person
  -> User
  -> Vector GH.Repo
  -> GitHubMetric
buildGitHubMetric person userInfo reposData =
  GitHubMetric (DP.entityKey person)
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
