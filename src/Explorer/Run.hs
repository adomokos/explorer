module Explorer.Run
  where

import Explorer.Import

import Data.List as L
import Data.Maybe (fromJust)
import Data.Ord ()
import qualified Data.Vector as DV
import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import Explorer.Entities
import qualified Explorer.GitHubProxy as GP
import GitHub (User(..))
import qualified GitHub as GH
import qualified RIO.Text as T

run :: RIO App ()
run = do
  let ghUsername = "adomokos"

  mPerson <- DB.runDb $ DB.fetchPersonByGhUsername ghUsername
  let person = fromJust mPerson
  -- logInfo $ displayShow person

  res <- GP.fetchUserAndRepos ghUsername

  -- We should use our own custom ADT error here
  let (userInfo, reposData) = either (error "no gitHub Info") id res

  let gitHubMetric = buildGitHubMetric person userInfo reposData

  -- logInfo $ displayShow gitHubMetric

  DB.runDb $ do
    DP.insert_ gitHubMetric
    ghMetricCount <- DB.countGHMetrics

    lift . logInfo $ "Number of ghMetrics: " <> displayShow ghMetricCount

buildGitHubMetric
  :: DP.Entity Person
  -> User
  -> Vector GH.Repo
  -> GitHubMetric
buildGitHubMetric person userInfo reposData =
  -- let repos = DV.toList $ DV.map (\x -> (GH.repoName x, GH.repoStargazersCount x)) reposData
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
    take 3 . L.sortOn (Down . snd) . DV.toList . DV.map (\x -> (GH.repoName x, GH.repoStargazersCount x)) $ reposData
