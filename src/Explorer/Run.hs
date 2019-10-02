{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Explorer.Run
  -- ( run
  -- )
           where

import Explorer.Import

import Data.Maybe (fromJust)
import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import Explorer.Entities
import GitHub (OwnerType(..), URL(..), User(..))
import qualified GitHub as GH
import qualified Explorer.GitHubProxy as GP
import qualified RIO.Text as T
import Explorer.Util (showEither)

run :: RIO App ()
run = do
  (user, _repos) <- GP.fetch2Data (GP.fetchUser "adomokos")
                                  (GP.fetchRepos "adomokos")

  logInfo $ showEither user

  let -- user = fetchUser'
      eGitHubInfo =
        (\x -> GitHubInfo (PersonKey 1)
                          (show $ userLogin x)
                          (T.unpack $ fromJust $ userName x)
                          (userCreatedAt x)
          )
          <$> user

      gitHubInfo = either (error "no gitHubInfo") id eGitHubInfo

  DB.runDb $ do
    DP.insert_ gitHubInfo
    pplCount <- DB.countPeople

    lift . logInfo $ "Number of ppl: " <> displayShow pplCount

  -- DB.run

fetchUser' :: Either GH.Error User
fetchUser' = Right $ User
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

