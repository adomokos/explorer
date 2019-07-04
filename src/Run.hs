{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Run
  -- ( run
  -- )
           where

import Import

import qualified DB
import GitHub (OwnerType(..), URL(..), User(..))
import qualified GitHub as GH
import qualified GitHubProxy as GP
import qualified Database.Persist.Sqlite as DP
import Util (showEither)
import Entities
import Data.Maybe (fromJust)
import qualified RIO.Text as T

run :: RIO App ()
run = do
  (user, _repos) <- GP.fetch2Data (GP.fetchUser "adomokos")
                                  (GP.fetchRepos "adomokos")

  logInfo $ showEither  (Right fetchUser' :: Either String User)

  let -- user = fetchUser'
      eGitHubInfo = (\x -> GitHubInfo (PersonKey 1)
                              (show $ userLogin x)
                              (T.unpack $ fromJust $ userName x)
                              (userCreatedAt x)) <$> user

      gitHubInfo = either (error "no gitHubInfo") id eGitHubInfo

  DB.runDb $ DP.insert_ gitHubInfo

  -- DB.run

fetchUser' :: User
fetchUser' = User
  { userId          = GH.mkId (Proxy :: Proxy User) 80530 -- :: GH.Id User
  , userLogin       = "adomokos"
  , userName        = Just "Attila Domokos"
  , userType        = OwnerUser
  , userCreatedAt   = DB.dummyDate
  , userPublicGists = 35
  , userAvatarUrl   = URL "https://avatars3.githubusercontent.com/u/80530?v=4"
  , userFollowers   = 95
  , userFollowing   = 9
  , userHireable    = Nothing
  , userBlog        = Just "https://www.adomokos.com"
  , userBio         = Nothing
  , userPublicRepos = 79
  , userLocation    = Just "Chicago, IL"
  , userCompany     = Nothing
  , userEmail       = Nothing
  , userUrl         = URL "https://api.github.com/users/adomokos"
  , userHtmlUrl     = URL "https://github.com/adomokos"
  }

