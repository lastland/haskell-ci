{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Job where

import Control.Lens
import Control.Monad.Freer

import Persistance

data Executor = Docker | Machine | MacOS

data Step :: * -> * where
  Run :: String -> Step ()
  Checkout :: Step ()
  SaveCache :: String -> [String] -> Step ()
  RestoreCache :: String -> Step ()
  PersistToWorkspace :: Workspace -> Step PersistanceHandle
  AttachWorkspace :: PersistanceHandle -> Step ()

type Steps a = Eff '[Step] a

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

attachWorkspace :: PersistanceHandle -> Steps ()
attachWorkspace = send . AttachWorkspace


data Job a = Job {
  _name :: String,
  _executor :: Executor,
  _steps :: Steps a
  }
  deriving Functor

makeLenses ''Job
