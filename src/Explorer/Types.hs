{-# LANGUAGE DeriveGeneric #-}
module Explorer.Types where

import Data.Aeson
  (ToJSON, defaultOptions, fieldLabelModifier, genericToEncoding, toEncoding)
import Data.Char (toLower)
import Data.Text (pack, replace, unpack)
import qualified Database.Persist.Sqlite as DB
import Explorer.Util (applyFirst)
import GHC.Generics (Generic)
import RIO
import RIO.Process

data AppError
  = UserNotFound Text
  | GitHubQueryFailed Text
  deriving (Show, Eq)

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  , appConnPool :: !DB.ConnectionPool
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

class HasConnPool env where
  connPoolL :: Lens' env DB.ConnectionPool

instance HasConnPool App where
  connPoolL = lens appConnPool (\x y -> x { appConnPool = y })

data RepoMetric = RepoMetric
  { repoMetricName :: String
  , repoMetricStargazerCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON RepoMetric where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier =
      applyFirst toLower . unpack . replace "repoMetric" "" . pack
  }
