{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Job (Executor(..),
            DependencyToken,
            Step(..), Steps,
            Job(..),
            runStep,
            checkout,
            saveCache,
            restoreCache,
            persistToWorkspace,
            attachWorkspace,
            createDependency,
            name, executor, steps,
            buildJobConfig) where

import Control.Lens

import Freer
import Persistance
import Executor
import qualified JobConfig

newtype DependencyToken = DependencyToken ()

data Step :: * -> * where
  Run :: String -> Step ()
  Checkout :: Step ()
  SaveCache :: String -> [String] -> Step ()
  RestoreCache :: String -> Step ()
  PersistToWorkspace :: Workspace -> Step PersistanceHandle
  AttachWorkspace :: PersistanceHandle -> Step Workspace
  Dependency :: Step DependencyToken

type Steps a = Freer Step a

runStep :: String -> Steps ()
runStep = send . Run

checkout :: Steps ()
checkout = send Checkout

saveCache :: String -> [String] -> Steps ()
saveCache s = send . (SaveCache s)

restoreCache :: String -> Steps ()
restoreCache = send . RestoreCache

persistToWorkspace :: Workspace -> Steps PersistanceHandle
persistToWorkspace = send . PersistToWorkspace

attachWorkspace :: PersistanceHandle -> Steps Workspace
attachWorkspace = send . AttachWorkspace

createDependency :: Steps DependencyToken
createDependency = send Dependency

data Job a = Job {
  _name :: String,
  _executor :: Executor,
  _steps :: Steps a
  }
  deriving Functor

makeLenses ''Job

buildStepConfig :: Step a -> (a, Maybe JobConfig.StepConfig)
buildStepConfig (Run s) = ((), Just $ JobConfig.Run s)
buildStepConfig Checkout = ((), Just JobConfig.Checkout)
buildStepConfig (SaveCache k ps) = ((), Just $ JobConfig.SaveCache k ps)
buildStepConfig (RestoreCache k) = ((), Just $ JobConfig.RestoreCache k)
buildStepConfig (PersistToWorkspace w) =
  (h, Just $ JobConfig.PersistToWorkspace (w ^. root) (w ^. paths))
  where h = persist w
buildStepConfig (AttachWorkspace h) =
  (w, Just $ JobConfig.AttachWorkspace as)
  where w  = restore h
        as = fmap (\p -> w ^. root ++ p) $ w ^. paths
buildStepConfig Dependency = (DependencyToken (), Nothing)

buildStepsConfig :: Steps a -> (a, [JobConfig.StepConfig])
buildStepsConfig (Done a) = (a, [])
buildStepsConfig (Eff f k) =
  let (a, c)    = buildStepConfig f in
  let (rst, cs) = buildStepsConfig (k a) in
    case c of
      Just c -> (rst, c : cs)
      _      -> (rst, cs)

buildJobConfig :: Job a -> (a, JobConfig.JobConfig)
buildJobConfig j = (rst, JobConfig.JobConfig (j ^. Job.name) (j ^. Job.executor) cs)
  where (rst, cs) = (buildStepsConfig $ j ^. Job.steps)
