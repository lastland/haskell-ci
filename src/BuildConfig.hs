{-# LANGUAGE GADTs #-}
module BuildConfig where

import Control.Lens
import Data.Sequence.Internal()
import Data.IORef
import GHC.Exts

import Freer
import qualified Resumption
import Persistance
import qualified Job
import JobConfig
import qualified Workflow
import WorkflowConfig

buildStepConfig :: Job.Step a -> (a, StepConfig)
buildStepConfig (Job.Run s) = ((), Run s)
buildStepConfig Job.Checkout = ((), Checkout)
buildStepConfig (Job.SaveCache k ps) = ((), SaveCache k ps)
buildStepConfig (Job.RestoreCache k) = ((), RestoreCache k)
buildStepConfig (Job.PersistToWorkspace w) = (h, PersistToWorkspace (w ^. root) (w ^. paths))
  where h = persist w
buildStepConfig (Job.AttachWorkspace h) = (w, AttachWorkspace as)
  where
    w  = restore h
    as = fmap (\p -> w ^. root ++ p) $ w ^. paths

buildStepsConfig :: Job.Steps a -> (a, [StepConfig])
buildStepsConfig (Done a) = (a, [])
buildStepsConfig (Eff f k) =
  let (a, c)    = buildStepConfig f in
  let (rst, cs) = buildStepsConfig (k a) in
  (rst, c : cs)

buildJobConfig :: Job.Job a -> (a, JobConfig)
buildJobConfig j = (rst, JobConfig (j ^. Job.name) (j ^. Job.executor) cs)
  where (rst, cs) = (buildStepsConfig $ j ^. Job.steps)

buildJobsConfig :: Resumption.Cont Job.Job a -> IO [[JobConfig]]
buildJobsConfig (Resumption.Cont ir) = do
  r <- ir
  case r of
    Resumption.Done _      -> return []
    Resumption.Blocked s c -> do
      l <- mapM g (toList s)
      ls <- buildJobsConfig c
      return $ l : ls
      where g :: Resumption.Blocks Job.Job -> IO JobConfig
            g (Resumption.Blocks e box) = do
              let (a, c) = buildJobConfig e
              writeIORef box (Just a)
              return c

buildWorkflowConfig :: Workflow.Workflow a -> IO WorkflowConfig
buildWorkflowConfig (Workflow.Workflow n js) = do
  cs <- buildJobsConfig js
  return $ WorkflowConfig n cs
