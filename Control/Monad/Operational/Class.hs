{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP, ConstraintKinds, FlexibleContexts #-}
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
-- Just for compatibility
----------------------------------------------------------------------------
module Control.Monad.Operational.Class ((:!), singleton) where

import Control.Elevator

type t :! m = Elevate t m

singleton :: Elevate f g => f a -> g a
singleton = elevate