{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Entities where

import qualified Data.Time as DT
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
Person sql=people
  username String
  firstname String
  lastname String
  UniquePerson username
  deriving Show
  deriving Eq

GitHubInfo sql=github_info
  person PersonId sql=people_id
  login String
  name String
  accountCreatedAt DT.UTCTime
  UniqueGitHubInfo login
  deriving Show
  deriving Eq
|]
