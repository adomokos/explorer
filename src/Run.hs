{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Run
  -- ( run
  -- )
           where

import Import

import Data.Maybe (fromJust)
import qualified Database.Persist.Sqlite as DP
import qualified DB
import Entities
import GitHub (OwnerType(..), URL(..), User(..))
import qualified GitHub as GH
import qualified GitHubProxy as GP
import qualified RIO.Text as T
import Util (showEither)

data AppError =
    GitHubFetchDataError
  | GitHubInfoBuildError
  deriving (Show)

run :: RIO App ()
run = do
  (user, repos) <- GP.fetch2Data (GP.fetchUser "adomokos")
                                 (GP.fetchRepos "adomokos")

  logInfo $ showEither user

  let -- user = fetchUser'
      eGitHubInfo' = buildGitHubInfo
        =<< GP.fetch2Data (GP.fetchUser "adomokos") (GP.fetchRepos "adomokos")

      eGitHubInfo = buildGitHubInfo =<< user
      gitHubInfo  = either (error "no gitHubInfo") id eGitHubInfo

  DB.runDb $ do
    DP.insert_ gitHubInfo
    pplCount <- DB.countPeople

    lift . logInfo $ "Number of ppl: " <> displayShow pplCount

  -- DB.run

buildGitHubInfo' :: (GH.User, Vector GH.Repo) -> Either GH.Error GitHubInfo
buildGitHubInfo' (user, _repos) = pure $ GitHubInfo
  (PersonKey 1)
  (show $ userLogin user)
  (T.unpack $ fromJust $ userName user)
  (userCreatedAt user)

buildGitHubInfo :: GH.User -> Either GH.Error GitHubInfo
buildGitHubInfo user = Right $ GitHubInfo
  (PersonKey 1)
  (show $ userLogin user)
  (T.unpack $ fromJust $ userName user)
  (userCreatedAt user)

fetchUser' :: Either GH.Error User
fetchUser' = Right $ User
  { userId          = GH.mkId (Proxy :: Proxy User) 80530 -- :: GH.Id User
  , userLogin       = "adomokos"
  , userName        = Just "Attila Domokos"
  , userType        = OwnerUser
  , userCreatedAt   = DB.dummyDate
  , userPublicGists = 35
  , userAvatarUrl   = URL "https://avatars3.githubusercontent.com/u/80530?v=4"
  , userFollowers   = 95
  , userFollowing   = 9
  , userHireable    = Nothing
  , userBlog        = Just "https://www.adomokos.com"
  , userBio         = Nothing
  , userPublicRepos = 79
  , userLocation    = Just "Chicago, IL"
  , userCompany     = Nothing
  , userEmail       = Nothing
  , userUrl         = URL "https://api.github.com/users/adomokos"
  , userHtmlUrl     = URL "https://github.com/adomokos"
  }

