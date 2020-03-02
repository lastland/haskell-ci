{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Freer where

import Control.Monad

data Freer :: (* -> *) -> * -> * where
  Done :: a -> Freer f a
  Eff  :: f a -> (a -> Freer f b) -> Freer f b
  
deriving instance Functor (Freer f)

instance Applicative (Freer f) where
  pure  = Done
  (<*>) = ap

instance Monad (Freer f) where
  return = pure
  Done x  >>= f = f x
  Eff e k >>= f = Eff e (\x -> k x >>= f)

send :: f a -> Freer f a
send f = Eff f Done
