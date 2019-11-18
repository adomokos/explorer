module Explorer.Run
  where

import Explorer.Import

import Data.Maybe (fromJust)
import qualified Database.Persist.Sqlite as DP
import qualified Explorer.DB as DB
import Explorer.Entities
import qualified Explorer.GitHubProxy as GP
import Explorer.Util (showEither)
import GitHub (OwnerType(..), URL(..), User(..))
import qualified GitHub as GH
import qualified RIO.Text as T

run :: RIO App ()
run = do
  let ghUsername = "adomokos"

  person <- DB.runDb $ DB.fetchPersonByGhUsername ghUsername
  logInfo $ displayShow person

  res <- GP.fetchUserAndRepos ghUsername

  -- We should use our own custom ADT error here
  let (userInfo, _repos) = either (error "no gitHub Info") id res

  let gitHubInfo =
        GitHubInfo (PersonKey 2)
                   (show $ userLogin userInfo)
                   (T.unpack $ fromJust $ userName userInfo)
                   (userPublicRepos userInfo)
                   (userCreatedAt userInfo)


  logInfo $ displayShow gitHubInfo

  -- DB.runDb $ do
    -- DP.insert_ gitHubInfo
    -- pplCount <- DB.countPeople

    -- lift . logInfo $ "Number of ppl: " <> displayShow pplCount

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

