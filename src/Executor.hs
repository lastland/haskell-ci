{-# LANGUAGE TemplateHaskell #-}
module Executor where

import Control.Lens

data Executor
  = Docker  { _dockerImage :: String }
  | Machine { _machineImage :: String,
              _docker_layer_caching :: Bool }
  | MacOS   { _xcode :: String }
  deriving Show

makeLenses ''Executor

docker :: String -> Executor
docker = Docker

machine :: String -> Executor
machine s = Machine s False

macos :: String -> Executor
macos  = MacOS
