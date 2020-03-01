{-# LANGUAGE TemplateHaskell #-}

module Persistance
  (Workspace(..),
    root, paths,
    PersistanceHandle,
    PersistanceMap,
    persist, restore) where

import Control.Lens hiding ((|>))
import Control.Monad.State
import Data.Sequence
import Data.Maybe

data Workspace = Workspace {
  _root :: String,
  _paths :: [String]
  }
  deriving Show
makeLenses ''Workspace

newtype PersistanceHandle = PersistanceHandle Int
  deriving Show

newtype PersistanceMap = PersistanceMap (Seq Workspace)

persist :: Workspace -> State PersistanceMap PersistanceHandle
persist w = do
  PersistanceMap m <- get
  put $ PersistanceMap (m |> w)
  return $ PersistanceHandle $ Data.Sequence.length m - 1

restore :: PersistanceHandle -> State PersistanceMap Workspace
restore (PersistanceHandle h) = do
  PersistanceMap m <- get
  return $ fromJust (m !? h) -- We can only get handle from insertion, so this is fine
