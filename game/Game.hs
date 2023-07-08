{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Game where

import Control.Concurrent.Async as Async
import Client
import Field (displayWaitPlayer2Window)

main :: IO ()
main = do
    _ <- Async.concurrently launchClientHandler displayWaitPlayer2Window
    return ()
