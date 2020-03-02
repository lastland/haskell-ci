{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Resumption where

import Data.IORef
import Data.Sequence

-- From paper "There is no Fork: an Abstraction for Efficient, Concurrent, and
-- Concise Data Access", with slight modification.

data Blocks f =
  forall a . Blocks (f a) (IORef (Maybe a))

newtype Cont f a = Cont { runCont :: IO (Resumption f a) }
  deriving Functor

data Resumption f a
  = Done a
  | Blocked (Seq (Blocks f)) (Cont f a)
deriving instance Functor (Resumption f)


instance Applicative (Cont f) where
  pure a = Cont $ return (pure a)
  Cont x <*> Cont y = Cont $ do
    x' <- x
    y' <- y
    return (x' <*> y')

instance Applicative (Resumption f) where
  pure = Done
  Done f <*> Done a = Done (f a)
  Done f <*> Blocked a k = Blocked a (f <$> k)
  Blocked a k <*> Done b = Blocked a (k <*> pure b)
  Blocked a k <*> Blocked b k' = Blocked (a <> b) (k <*> k')


instance Monad (Cont f) where
  return = pure
  Cont x >>= k = Cont $ do
    x' <- x
    case x' of
      Done a       -> runCont $ k a
      Blocked a k' -> return $ Blocked a (k' >>= k)

lift :: f a -> Cont f a
lift f = Cont $ do
  box <- newIORef $ Nothing
  let bl   = Blocks f box
  let cont = Cont $ do
        Just a <- readIORef box
        return (Done a)
  return (Blocked (singleton bl) cont)
                
