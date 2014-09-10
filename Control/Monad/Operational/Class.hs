{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Operational.Class
-- Copyright   :  (C) 2012-2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A class for operational monads
----------------------------------------------------------------------------
module Control.Monad.Operational.Class ((:!)(..)) where

import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Error
#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except
#endif
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Data.Monoid

class Monad m => t :! m | m -> t where
  -- | Construct an operational action from a single imperative.
  singleton :: t a -> m a

instance (t :! m) => t :! ReaderT e m where
  singleton = lift . singleton

instance (t :! m) => t :! Lazy.StateT s m where
  singleton = lift . singleton

instance (t :! m) => t :! Strict.StateT s m where
  singleton = lift . singleton

instance (t :! m) => t :! ContT r m where
  singleton = lift . singleton

instance (t :! m, Monoid w) => t :! Lazy.WriterT w m where
  singleton = lift . singleton

instance (t :! m, Monoid w) => t :! Strict.WriterT w m where
  singleton = lift . singleton

instance (t :! m, Monoid w) => t :! Strict.RWST r w s m where
  singleton = lift . singleton

instance (t :! m, Monoid w) => t :! Lazy.RWST r w s m where
  singleton = lift . singleton

instance (t :! m) => t :! MaybeT m where
  singleton = lift . singleton

instance (t :! m) => t :! IdentityT m where
  singleton = lift . singleton

instance (t :! m) => t :! ListT m where
  singleton = lift . singleton

instance (t :! m, Error e) => t :! ErrorT e m where
  singleton = lift . singleton

#if MIN_VERSION_transformers(0,4,0)
instance (t :! m) => t :! ExceptT e m where
  singleton = lift . singleton
#endif