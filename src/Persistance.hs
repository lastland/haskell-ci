{-# LANGUAGE TemplateHaskell #-}

module Persistance
  (Workspace(..),
    root, paths,
    PersistanceHandle,
    persist, restore) where

import Control.Lens hiding ((|>))

data Workspace = Workspace {
  _root :: String,
  _paths :: [String]
  }
  deriving Show
makeLenses ''Workspace

newtype PersistanceHandle = PersistanceHandle Workspace
  deriving Show

persist :: Workspace -> PersistanceHandle
persist w = PersistanceHandle w

restore :: PersistanceHandle -> Workspace
restore (PersistanceHandle w) = w
