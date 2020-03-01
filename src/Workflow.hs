{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Workflow where

import Control.Lens

import Resumption
import Job

data Workflow a = Workflow {
  _name :: String,
  _jobs :: Resumption Job a
  } deriving Functor

makeLenses ''Workflow
