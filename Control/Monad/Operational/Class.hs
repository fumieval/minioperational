{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
module Control.Monad.Operational.Class (Operational(..)) where

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
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Data.Monoid

class Monad m => Operational t m | m -> t where
  -- | Construct an operational action from a single imperative.
  singleton :: t a -> m a

instance (Operational f m) => Operational f (ReaderT e m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (Lazy.StateT s m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (Strict.StateT s m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (ContT r m) where
  singleton = lift . singleton

instance (Operational f m, Monoid w) => Operational f (Lazy.WriterT w m) where
  singleton = lift . singleton

instance (Operational f m, Monoid w) => Operational f (Strict.WriterT w m) where
  singleton = lift . singleton

instance (Operational f m, Monoid w) => Operational f (Strict.RWST r w s m) where
  singleton = lift . singleton

instance (Operational f m, Monoid w) => Operational f (Lazy.RWST r w s m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (MaybeT m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (IdentityT m) where
  singleton = lift . singleton

instance (Operational f m) => Operational f (ListT m) where
  singleton = lift . singleton

instance (Operational f m, Error e) => Operational f (ErrorT e m) where
  singleton = lift . singleton