{-# LANGUAGE Rank2Types, GADTs, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Operational.Mini
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Simple operational monad from a free monad
----------------------------------------------------------------------------
module Control.Monad.Operational.Mini (Program, interpret, module Control.Monad.Operational.Class) where

import Data.Functor.Yoneda.Contravariant
import Control.Monad.Free.Church
import Control.Monad.Operational.Class

type Program t = F (Yoneda t)

interpret :: Monad m => (forall x. t x -> m x) -> Program t a -> m a
interpret e (F m) = m return (\(Yoneda f t) -> e t >>= f)

instance Operational t (Program t) where
    singleton = liftF . liftYoneda