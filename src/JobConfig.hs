{-# LANGUAGE TemplateHaskell #-}
module JobConfig where

import Control.Lens

import Job

data StepConfig
  = Run String
  | Checkout
  | SaveCache String [String]
  | RestoreCache String
  | PersistToWorkspace String [String]
  | AttachWorkspace [String]
  deriving Show
makePrisms ''StepConfig

data JobConfig = JobConfig {
  _name :: String,
  _executor :: Executor,
  _steps :: [StepConfig]
  } deriving Show
makeLenses ''JobConfig
