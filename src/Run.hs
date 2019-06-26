{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import Data.List (intercalate)
import qualified Data.Vector as DV
import qualified GitHub as GH
import qualified GitHub.Endpoints.Users.PublicSSHKeys as PK
import Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  fetchUser "adomokos"
  fetchPublicSSHKey "adomokos"

fetchPublicSSHKey :: GH.Name GH.Owner -> RIO App ()
fetchPublicSSHKey username = do
  ePublicSSHKeys <- liftIO $ PK.publicSSHKeysFor' username
  case ePublicSSHKeys of
    (Left  err          ) -> logInfo $ "Error: " <> displayShow err
    (Right publicSSHKeys) -> logInfo . displayShow $ intercalate "\n" $ map
      show
      (DV.toList publicSSHKeys)

fetchUser :: GH.Name GH.User -> RIO App ()
fetchUser username = do
  eUser <- liftIO . GH.executeRequest' $ GH.userInfoForR username

  case eUser of
    Left  err  -> logInfo $ "Error: " <> displayShow err
    Right user -> logInfo . displayShow $ user
