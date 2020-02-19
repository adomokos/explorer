module Explorer.GitHubProxy where

import Explorer.Import

import qualified Data.Vector as DV
import Explorer.DB as DB
import Explorer.Util (fetch2Data, fetch3Data, showEither)
import GitHub (OwnerType(..), URL(..), User(..))
import qualified GitHub as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK

-- | Run functions from this module
run :: RIO App ()
run = do
  (user, sshKeys, repos) <- fetch3Data (fetchUser "adomokos")
                                       (fetchPublicSSHKeys "adomokos")
                                       -- (fetchRepo "adomokos" "light-service")
                                       (fetchRepos "adomokos")

  logInfo $ showEither user
  logInfo $ showEither sshKeys
  -- logInfo $ showEither repo
  logInfo $ showEither repos

fetchPublicSSHKeys
  :: (MonadIO m)
  => GH.Name GH.Owner
  -> m (Either GH.Error (DV.Vector GH.PublicSSHKeyBasic))
fetchPublicSSHKeys owner = liftIO $ GH.github' PK.publicSSHKeysForR owner GH.FetchAll

fetchRepo
  :: (MonadIO m)
  => GH.Name GH.Owner
  -> GH.Name GH.Repo
  -> m (Either GH.Error GH.Repo)
fetchRepo username repo = liftIO $ GH.github' GH.repositoryR username repo

fetchUser :: (MonadIO m) => GH.Name GH.User -> m (Either GH.Error GH.User)
fetchUser = liftIO . GH.github' . GH.userInfoForR

fetchRepos
  :: (MonadIO m) => GH.Name GH.Owner -> m (Either GH.Error (Vector GH.Repo))
fetchRepos username = liftIO $
  GH.github' GH.userReposR username GH.RepoPublicityOwner GH.FetchAll

fetchUserAndRepos
  :: (MonadUnliftIO m)
  => Text
  -> m (Either GH.Error (GH.User, Vector GH.Repo))
fetchUserAndRepos username = do
  let githubUsername = GH.mkUserName username
  (u,r) <- fetch2Data
    (fetchUser githubUsername)
    (fetchRepos $ GH.fromUserName githubUsername)
  pure $ (,) <$> u <*> r

-- | Used for testing, will move it to specs
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

