--
-- Copyright (c) 2012 Citrix Systems, Inc.
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

{-# LANGUAGE ScopedTypeVariables #-}

module Tools.Future 
    ( Future
    , future
    , futureL
    , force
    ) where

import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.Trans
import System.IO.Unsafe

import Tools.FreezeIOM

-- | Simple futures in freezeable monads
newtype Future m a
      = Future { force :: m a }

future :: (MonadIO m, FreezeIOM context i m) => m a -> m (Future m a) 
future action
  = freeze $ \ctx -> do
         r <- newEmptyMVar
         forkIO $ compute ctx r
         return . Future $ cont =<< extract r
    where
      compute ctx r
          = ( thaw ctx action >>= putMVar r . Right ) `E.catch` err
            where err ex = putMVar r (Left (ex :: E.SomeException))
      extract r
          = liftIO (readMVar r >>= verify)
            where verify (Left ex) = E.throw ex
                  verify (Right x) = return x

-- | lazily force the future on use (unsafe)
futureL :: (MonadIO m, FreezeIOM context i m) => m a -> m a
futureL action
    = do f <- future action
         (freeze $ \ctx ->
              unsafeInterleaveIO (thaw ctx $ force f)) >>= cont

instance Functor m => Functor (Future m) where
    f `fmap` (Future x) = Future (f `fmap` x)

instance Applicative m => Applicative (Future m) where
    pure x = Future (pure x)
    Future f <*> Future x = Future $ f <*> x

{- example:

sleep f = threadDelay (round $ f*10^6)
main = do
  a <- future $ sleep 0.3 >> return 1
  b <- future $ sleep 1.0 >> return 2
  c <- force ((+) <$> a <*> b)
  putStrLn (show c)

-}

