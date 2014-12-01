{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, GADTs, TypeOperators, DataKinds, TypeFamilies, ConstraintKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Operational.Mini
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  RankNTypes
--
-- Simple operational monad
----------------------------------------------------------------------------
module Control.Monad.Operational.Mini (Program(..)
    , interpret
    , cloneProgram
    , ReifiedProgram(..)
    , fromReified
    , module Control.Monad.Operational.Class
    , module Control.Monad.Operational.TH) where

import Control.Monad.Operational.Class
import Control.Monad.Operational.TH
import Data.OpenUnion1.Clean
import Control.Elevator
import Control.Applicative
import Data.Functor.Identity

infixl 1 :>>=

-- | Program t is a 'Monad' that represents a sequence of imperatives.
-- To construct imperatives, use 'singleton' :: t a -> Program t a.
newtype Program t a = Program { unProgram :: forall r. (a -> r) -> (forall x. t x -> (x -> r) -> r) -> r }

instance Functor (Program t) where
    fmap f (Program m) = Program $ \p i -> m (p . f) i

instance Applicative (Program t) where
    pure a = Program $ \p _ -> p a
    Program mf <*> Program ma = Program $ \p i -> mf (\f -> ma (p . f) i) i

instance Monad (Program t) where
    return a = Program $ \p _ -> p a
    Program m >>= k = Program $ \p i -> m (\a -> unProgram (k a) p i) i

-- | Interpret a 'Program' using the given transformation.
interpret :: Monad m => (forall x. t x -> m x) -> Program t a -> m a
interpret e (Program m) = m return (\t f -> e t >>= f)

cloneProgram :: (Monad m, Elevate t m) => Program t a -> m a
cloneProgram (Program m) = m return ((>>=) . elevate)

instance Tower (Program t) where
    type Floors (Program t) = t :> ReifiedProgram t :> Identity :> Empty
    toLoft = (\t -> Program $ \p i -> i t p) ||> fromReified ||> pure . runIdentity ||> exhaust

-- | Reified version of 'Program'. It is useful for testing.
data ReifiedProgram t a where
    Return :: a -> ReifiedProgram t a
    (:>>=) :: t a -> (a -> ReifiedProgram t b) -> ReifiedProgram t b

fromReified :: ReifiedProgram t a -> Program t a
fromReified m = Program $ \p i ->
    let go (Return a) = p a
        go (t :>>= c) = i t (go . c) in go m

instance Functor (ReifiedProgram t) where
    fmap f = go where
        go (Return a) = Return (f a)
        go (t :>>= k) = t :>>= go . k
    {-# INLINE fmap #-}

instance Applicative (ReifiedProgram t) where
    pure = Return
    {-# INLINE pure #-}
    Return f <*> Return a = Return (f a)
    mf <*> m = mf >>= \f -> fmap f m

instance Monad (ReifiedProgram t) where
    return = Return
    {-# INLINE return #-}
    Return a >>= f = f a
    (t :>>= m) >>= k = t :>>= (>>= k) . m

instance Tower (ReifiedProgram t) where
    type Floors (ReifiedProgram t) = t :> Program t :> Identity :> Empty
    toLoft = (:>>= Return) ||> (\(Program m) -> Return (:>>=)) ||> pure . runIdentity ||> exhaust