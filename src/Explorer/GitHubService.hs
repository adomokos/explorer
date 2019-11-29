module Explorer.GitHubService where

import Explorer.Import

import Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Vector as DV
import qualified Database.Persist.Sqlite as DP
import Explorer.Entities
import qualified Explorer.GitHubProxy as GHP
import GitHub (User(..))
import qualified GitHub as GH
import qualified RIO.Text as T

retrieveGitHubMetric
  :: MonadUnliftIO m
  => DP.Entity Person -> m (Either AppError GitHubMetric)
retrieveGitHubMetric (DP.Entity personKey person) = do
  res <- GHP.fetchUserAndRepos $ T.pack $ personGitHubUsername person

  pure $
    either (Left . GitHubQueryFailed . T.pack . show)
           (Right . uncurry (buildGitHubMetric personKey))
           res

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
