{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  -- ( run
  -- )
           where

import qualified GitHubProxy as GP
import Import

run :: RIO App ()
run = do
  (user, sshKeys, repo) <- GP.runAsync
    (GP.fetchUser "adomokos")
    (GP.fetchPublicSSHKeys "adomokos")
    (GP.fetchRepo "adomokos" "light-service")
    (GP.fetchRepos "adomokos")

  logInfo $ showEither user
  logInfo $ showEither sshKeys
  logInfo $ showEither repo
  -- logInfo $ showEither repos

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Error: " <> displayShow err
  Right value -> displayShow value
