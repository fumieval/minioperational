{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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
module Control.Monad.Trans.Operational.Mini (ProgramT(..), interpret, module Control.Monad.Operational.Class) where

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