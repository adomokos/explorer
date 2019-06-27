{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  -- ( run
  -- )
  where

import qualified Data.Vector as DV
import qualified GitHub as GH
import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import Import

run :: RIO App ()
run = do
  (user, sshKeys, repo) <- runAsync (fetchUser "adomokos")
                                    (fetchPublicSSHKeys "adomokos")
                                    (fetchRepo "adomokos" "light-service")

  logInfo $ showEither user
  logInfo $ showEither sshKeys
  logInfo $ showEither repo

runAsync :: MonadUnliftIO m => m a -> m b -> m c -> m (a, b, c)
runAsync action1 action2 action3 = do
  a1 <- async action1
  a2 <- async action2
  a3 <- async action3
  user <- wait a1
  sshKeys <- wait a2
  repo <- wait a3

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

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Error: " <> displayShow err
  Right value -> displayShow value
