{-# LANGUAGE NoImplicitPrelude #-}
module Explorer.Types where

import qualified Database.Persist.Sqlite as DB
import RIO
import RIO.Process

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
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

class HasConnPool env where
  connPoolL :: Lens' env DB.ConnectionPool

instance HasConnPool App where
  connPoolL = lens appConnPool (\x y -> x { appConnPool = y })
