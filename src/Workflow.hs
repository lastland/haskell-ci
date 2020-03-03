{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Workflow where

import Control.Lens
import Data.Sequence.Internal()
import Data.IORef
import GHC.Exts

import Resumption
import Job
import JobConfig
import WorkflowConfig

data Workflow a = Workflow {
  _name :: String,
  _jobs :: Cont Job a
  } deriving Functor

makeLenses ''Workflow

buildJobsConfig :: Cont Job a -> IO [[JobConfig]]
buildJobsConfig (Cont ir) = do
  r <- ir
  case r of
    Done _      -> return []
    Blocked s c -> do
      l <- mapM g (toList s)
      ls <- buildJobsConfig c
      return $ l : ls
      where g :: Blocks Job -> IO JobConfig
            g (Blocks e box) = do
              let (a, c) = buildJobConfig e
              writeIORef box (Just a)
              return c

buildWorkflowConfig :: Workflow a -> IO WorkflowConfig
buildWorkflowConfig (Workflow n js) = do
  cs <- buildJobsConfig js
  return $ WorkflowConfig n cs
