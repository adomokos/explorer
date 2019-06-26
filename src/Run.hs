{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import qualified GitHub as GH
import Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"

  eUser <- liftIO $ GH.executeRequest' $ GH.userInfoForR "adomokos"

  case eUser of
    Left  err  -> logInfo $ "error occurred: " <> displayShow err
    Right user -> logInfo . displayShow $ user
