--
-- Copyright (c) 2011 Citrix Systems, Inc.
-- 
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
--

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveDataTypeable #-}

module Tools.FreezeIOM
    ( FreezeIOM (..)
    ) where

-- | Monads 'freezeable' within IO,
-- | to allow context tunneling to IO callbacks and back
class Monad m => FreezeIOM context i m | m -> context, m -> i where
    -- | snapshot monad context and execute some IO
    freeze  :: (context -> IO a) -> m a
    -- | execute monad code from within IO yielding intermediate representation of result
    thaw    :: context -> m a -> IO (i a)
    -- | back in monad, continue from intermediate result
    cont    :: i a -> m a

-- no-op implementation for IO monad
newtype Wrap a
      = Wrap a

instance FreezeIOM () Wrap IO where
    freeze f        = f ()
    thaw   () f     = f >>= return . Wrap
    cont   (Wrap x) = return x
