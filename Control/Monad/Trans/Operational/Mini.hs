{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,KindSignatures, DataKinds, TypeFamilies, ConstraintKinds #-}
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
  ProgramT(..)
  , unProgram
  , cloneProgramT
  , interpret
  , ReifiedProgramT(..)
  , fromReifiedT
  , transReifiedT
  , hoistReifiedT
  , module Control.Monad.Operational.Class
  , module Control.Monad.Operational.TH
  ) where

import Control.Monad
import Control.Monad.Operational.Class
import Control.Monad.Operational.TH
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Elevator
import Data.OpenUnion1.Clean
import qualified Control.Monad.Operational.Mini as P

newtype ProgramT t m a = ProgramT
  { unProgramT :: forall r. (a -> r) -> (m r -> r) -> (forall x. t x -> (x -> r) -> r) -> r }

cloneProgramT :: (Monad n, Elevate t n, Elevate m n) => ProgramT t m a -> n a
cloneProgramT (ProgramT m) = m return (join . elevate) ((>>=) . elevate)

unProgram :: Monad m => ProgramT t m a -> (a -> m r) -> (forall x. t x -> (x -> m r) -> m r) -> m r
unProgram (ProgramT m) r b = m r join b

instance Functor (ProgramT t m) where
    fmap f (ProgramT m) = ProgramT $ \p l i -> m (p . f) l i

instance Applicative (ProgramT t m) where
    pure a = ProgramT $ \p _ _ -> p a
    ProgramT mf <*> ProgramT ma = ProgramT $ \p l i -> mf (\f -> ma (p . f) l i) l i

instance Monad (ProgramT t m) where
    return a = ProgramT $ \p _ _ -> p a
    ProgramT m >>= k = ProgramT $ \p l i -> m (\a -> unProgramT (k a) p l i) l i

-- | Interpret a 'Program' using the given transformation.
interpret :: Monad m => (forall x. t x -> m x) -> ProgramT t m a -> m a
interpret e (ProgramT m) = m return join (\t c -> e t >>= c)

instance (Monad m, Tower m) => Tower (ProgramT t m) where
    type Floors (ProgramT t m) = t
      :> ReifiedProgramT t m
      :> P.Program t
      :> P.ReifiedProgram t
      :> Floors1 m
    toLoft = (\t -> ProgramT $ \p _ i -> i t p)
      ||> fromReifiedT
      ||> P.cloneProgram
      ||> P.cloneProgram . P.fromReified
      ||> lift . toLoft1

instance MonadTrans (ProgramT t) where
    lift m = ProgramT $ \p l _ -> l (liftM p m)

infix 1 :>>=

data ReifiedProgramT t (m :: * -> *) a where
  Return :: a -> ReifiedProgramT t m a
  (:>>=) :: t a -> (a -> ReifiedProgramT t m b) -> ReifiedProgramT t m b
  Lift :: m a -> (a -> ReifiedProgramT t m b) -> ReifiedProgramT t m b

fromReifiedT :: Monad m => ReifiedProgramT t m a -> ProgramT t m a
fromReifiedT m = ProgramT $ \p l i ->
  let go (Return a) = p a
      go (t :>>= c) = i t (go . c)
      go (Lift a c) = l $ liftM (go . c) a
   in go m

transReifiedT :: Monad m => (forall x. m x -> n x) -> ReifiedProgramT t m a -> ReifiedProgramT t n a
transReifiedT _ (Return a) = Return a
transReifiedT t (i :>>= cont) = i :>>= transReifiedT t . cont
transReifiedT t (Lift m cont) = Lift (t m) (transReifiedT t . cont)

hoistReifiedT :: Monad m => (forall x. t x -> s x) -> ReifiedProgramT t m a -> ReifiedProgramT s m a
hoistReifiedT _ (Return a) = Return a
hoistReifiedT t (i :>>= cont) = t i :>>= hoistReifiedT t . cont
hoistReifiedT t (Lift m cont) = Lift m (hoistReifiedT t . cont)

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

instance (Monad m, Tower m) => Tower (ReifiedProgramT t m) where
    type Floors (ReifiedProgramT t m) = t
      :> ProgramT t m
      :> P.Program t
      :> P.ReifiedProgram t
      :> Floors1 m
    toLoft = (:>>= Return)
      ||> cloneProgramT
      ||> P.cloneProgram
      ||> P.cloneProgram . P.fromReified
      ||> lift . toLoft1

instance MonadTrans (ReifiedProgramT t) where lift = flip Lift Return
