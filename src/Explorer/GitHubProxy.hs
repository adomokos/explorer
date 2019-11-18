module Explorer.GitHubProxy where

import Explorer.Import

import qualified Data.Vector as DV
import Explorer.Util (fetch2Data, fetch3Data, showEither)
import qualified GitHub as GH
import qualified GitHub.Endpoints.Repos as GH
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
fetchPublicSSHKeys = liftIO . PK.publicSSHKeysFor'

fetchRepo
  :: (MonadIO m)
  => GH.Name GH.Owner
  -> GH.Name GH.Repo
  -> m (Either GH.Error GH.Repo)
fetchRepo username repo = liftIO $ GH.repository username repo

fetchUser :: (MonadIO m) => GH.Name GH.User -> m (Either GH.Error GH.User)
fetchUser = liftIO . GH.executeRequest' . GH.userInfoForR

fetchRepos
  :: (MonadIO m) => GH.Name GH.Owner -> m (Either GH.Error (Vector GH.Repo))
fetchRepos username = liftIO $ GH.userRepos username GH.RepoPublicityOwner

fetchUserAndRepos
  :: (MonadUnliftIO m)
  => GH.Name GH.User
  -> m (Either GH.Error (GH.User, Vector GH.Repo))
fetchUserAndRepos username = do
  (u,r) <- fetch2Data
    (fetchUser username)
    (fetchRepos $ GH.fromUserName username)
  pure $ (,) <$> u <*> r
