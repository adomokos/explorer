module Explorer.GitHubService where

import Explorer.Import

import Data.Aeson.Text (encodeToLazyText)
import Data.List as L
import Data.Maybe (fromJust)
import Data.Text.Lazy (toStrict)
import qualified Data.Vector as DV
import qualified Database.Persist.Sqlite as DP
import Explorer.Entities
import qualified Explorer.GitHubProxy as GHP
import GitHub (User(..))
import qualified GitHub as GH
import GitHub.Data.Name (untagName)
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
               (Just . toStrict . encodeToLazyText $ mostStarGazedRepos)
               (userCreatedAt userInfo)
 where
  mostStarGazedRepos =
    map buildRepoMetric
      . threeMostStargazed
      . DV.toList
      . DV.map extractField
      $ reposData
  buildRepoMetric (name, stargazerCount) =
    RepoMetric (T.unpack name) stargazerCount
  extractField repoInfo =
    (untagName . GH.repoName $ repoInfo, GH.repoStargazersCount repoInfo)
  threeMostStargazed :: [(Text, Int)] -> [(Text, Int)]
  threeMostStargazed = take 3 . L.sortOn (Down . snd)
