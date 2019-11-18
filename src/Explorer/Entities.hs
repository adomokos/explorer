{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Explorer.Entities where

import Explorer.Import

import qualified Data.Time as DT
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
Person sql=people
  email String
  firstname String
  lastname String
  githubUsername String
  UniquePerson email
  deriving Show
  deriving Eq

GitHubInfo sql=github_info
  person PersonId sql=people_id
  login String
  name String
  publicReposCount Int
  accountCreatedAt DT.UTCTime
  UniqueGitHubInfo login
  deriving Show
  deriving Eq
|]
