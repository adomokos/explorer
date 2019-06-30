{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Run
  -- ( run
  -- )
           where

import Import

import qualified DB
import qualified GitHubProxy as GP

run' :: RIO App ()
run' = do
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

run :: RIO App ()
run = do
  logInfo "Hello"

  pplCount <- DB.runDb DB.countPeople

  logInfo $ "Number of ppl: " <> displayShow pplCount
