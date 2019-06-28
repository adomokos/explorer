{-# LANGUAGE NoImplicitPrelude #-}
module GitHubProxy where

import qualified Data.Vector as DV
import qualified GitHub as GH
import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import Import

runAsync :: MonadUnliftIO m => m a -> m b -> m c -> m d -> m (a, b, c)
runAsync action1 action2 action3 action4 = do
  a1      <- async action1
  a2      <- async action2
  a3      <- async action3
  a4      <- async action4
  user    <- wait a1
  sshKeys <- wait a2
  repo    <- wait a3
  _repos  <- wait a4

  pure (user, sshKeys, repo)

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