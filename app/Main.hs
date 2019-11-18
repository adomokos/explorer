{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Explorer.DB as DB
import Explorer.Import
import Explorer.Run (run)
import LoadEnv (loadEnv)
import Options.Applicative.Simple
import qualified Paths_explorer
import RIO.Process

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_explorer.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext

  loadEnv

  withLogFunc lo $ \lf -> do
    dbPool <- DB.createDbPool
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appConnPool = dbPool
          }
     in runRIO app run
