{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import qualified Data.Vector as DV
import qualified GitHub as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  logInfo =<< showEither <$> fetchUser "adomokos"
  logInfo =<< showEither <$> fetchPublicSSHKey "adomokos"

fetchPublicSSHKey
  :: (MonadIO m)
  => GH.Name GH.Owner
  -> m (Either GH.Error (DV.Vector GH.PublicSSHKeyBasic))
fetchPublicSSHKey = liftIO . PK.publicSSHKeysFor'

fetchUser :: (MonadIO m) => GH.Name GH.User -> m (Either GH.Error GH.User)
fetchUser = liftIO . GH.executeRequest' . GH.userInfoForR

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Error: " <> displayShow err
  Right value -> displayShow value
