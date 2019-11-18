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
  gitHubUsername String
  UniquePerson email
  deriving Show
  deriving Eq

GitHubMetric sql=git_hub_metrics
  person PersonId sql=people_id
  login String
  name String
  publicGists Int
  publicRepos Int
  followers Int
  following Int
  mostStargazedRepos Text Maybe
  accountCreatedAt DT.UTCTime
  deriving Show
  deriving Eq
|]
