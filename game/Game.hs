{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Game where

import Client
import Field

testClient :: IO ()
testClient = do
  Client.sendConnect
  Client.sendConnect
  Client.sendChangePosition 2 (2, 2)
  Client.sendShoot (2, 2) (4, 4)

main :: IO ()
main = Field.displayWaitPlayer2Window
