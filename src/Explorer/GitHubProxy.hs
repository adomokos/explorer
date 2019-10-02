{-# LANGUAGE NoImplicitPrelude #-}
module Explorer.GitHubProxy where

import Explorer.Import

import qualified Data.Vector as DV
import qualified GitHub as GH
import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import Explorer.Util (showEither)

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

fetch2Data :: MonadUnliftIO m => m a -> m b -> m (a, b)
fetch2Data action1 action2 = do
  a1      <- async action1
  a2      <- async action2
  result1 <- wait a1
  result2 <- wait a2

  pure (result1, result2)

fetch3Data :: MonadUnliftIO m => m a -> m b -> m c -> m (a, b, c)
fetch3Data action1 action2 action3 = do
  a1      <- async action1
  a2      <- async action2
  a3      <- async action3
  -- a4      <- async action4
  result1 <- wait a1
  result2 <- wait a2
  result3 <- wait a3
  -- repos   <- wait a4

  pure (result1, result2, result3)

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
