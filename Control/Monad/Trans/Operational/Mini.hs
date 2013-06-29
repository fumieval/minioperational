{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,KindSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Operational.Mini
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  RankNTypes
--
-- Simple operational monad transformer
----------------------------------------------------------------------------
module Control.Monad.Trans.Operational.Mini (
  ProgramT(..), interpret,  ReifiedProgramT(..), fromReifiedT,
  module Control.Monad.Operational.Class
  ) where

import Control.Monad
import Control.Monad.Operational.Class
import Control.Applicative
import Control.Monad.Trans.Class

newtype ProgramT t m a = ProgramT { unProgram :: forall r. (a -> m r) -> (forall x. t x -> (x -> m r) -> m r) -> m r }

instance Functor (ProgramT t m) where
    fmap f (ProgramT m) = ProgramT $ \p i -> m (p . f) i

instance Applicative (ProgramT t m) where
    pure a = ProgramT $ \p _ -> p a
    ProgramT mf <*> ProgramT ma = ProgramT $ \p i -> mf (\f -> ma (p . f) i) i

instance Monad (ProgramT t m) where
    return a = ProgramT $ \p _ -> p a
    ProgramT m >>= k = ProgramT $ \p i -> m (\a -> unProgram (k a) p i) i

-- | Interpret a 'Program' using the given transformation.
interpret :: Monad m => (forall x. t x -> m x) -> ProgramT t m a -> m a
interpret e (ProgramT m) = m return (\t c -> e t >>= c)

instance Operational t (ProgramT t m) where
    singleton t = ProgramT $ \p i -> i t p

instance MonadTrans (ProgramT t) where
    lift m = ProgramT $ \p _ -> m >>= p


infix 1 :>>=

data ReifiedProgramT t (m :: * -> *) a where
  Return :: a -> ReifiedProgramT t m a
  (:>>=) :: t a -> (a -> ReifiedProgramT t m b) -> ReifiedProgramT t m b
  Lift :: m a -> (a -> ReifiedProgramT t m b) -> ReifiedProgramT t m b

fromReifiedT :: Monad m => ReifiedProgramT t m a -> ProgramT t m a
fromReifiedT m = ProgramT $ \p i ->
  let go (Return a) = p a
      go (t :>>= c) = i t (go . c)
      go (Lift a c) = a >>= go . c
   in go m


instance Monad m => Functor (ReifiedProgramT t m) where
    fmap f = go where
        go (Return a) = Return (f a)
        go (t :>>= k) = t :>>= go . k
        go (Lift a c) = Lift a (go.c)
    {-# INLINE fmap #-}

instance Monad m => Applicative (ReifiedProgramT t m) where
    pure = Return
    {-# INLINE pure #-}
    Return f <*> Return a = Return (f a)
    mf <*> m = mf >>= \f -> fmap f m

instance Monad m => Monad (ReifiedProgramT t m) where
    return = Return
    {-# INLINE return #-}
    Return a >>= f = f a
    (t :>>= m) >>= k = t :>>= (>>= k) . m
    Lift a c >>= f = Lift a (c >=> f)

instance Monad m => Operational t (ReifiedProgramT t m) where
    singleton t = t :>>= Return

instance MonadTrans (ReifiedProgramT t) where lift = flip Lift Return
