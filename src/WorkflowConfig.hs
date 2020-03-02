{-# LANGUAGE TemplateHaskell #-}
module WorkflowConfig where

import Control.Lens

import JobConfig

data WorkflowConfig = WorkflowConfig {
  _name :: String,
  _jobs :: [[JobConfig]]
  } deriving Show
makeLenses ''WorkflowConfig
