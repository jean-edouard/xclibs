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

{-# LANGUAGE FunctionalDependencies,MultiParamTypeClasses,FlexibleContexts,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances,DeriveFunctor #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Rpc.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as M
import Rpc.Types
import Rpc.Error
import Rpc.Remote
import Rpc.Dispatch

import qualified Network.DBus.Actions as D

import Tools.FreezeIOM

--
-- context
--
data RpcContext =
     RpcContext { domUClients   :: MVar (Map Int Dispatcher)
                , names         :: MVar [(String,Dispatcher)]
                , client        :: Dispatcher
                , messageSender :: Maybe BusName }

rpcMkContext :: Dispatcher -> IO RpcContext
rpcMkContext client =
    do b <- newMVar (M.empty)
       n <- newMVar []
       return $ RpcContext b n client Nothing

instance (IsRemoteError e) => Control.Monad.Error.Error e where
    noMsg    = undefined
    strMsg _ = undefined


--
-- RPC monad. Functions which call RPC services live in this monad.
--

class (
  Functor m,
  Applicative m,
  Monad m,
  MonadIO m,
  IsRemoteError e,
  MonadError e m
  ) => MonadRpc e m | m -> e where
        rpcGetContext   :: m RpcContext
        rpcLocalContext :: (RpcContext -> RpcContext) -> m a -> m a

newtype RpcM e a = RpcM {
      runRpcM_ :: ReaderT RpcContext (ErrorT e IO) a
    }
    deriving (Functor, Monad, MonadIO, MonadReader RpcContext, MonadError e)

runRpcM :: RpcM e a -> RpcContext -> IO (Either e a)
runRpcM f ctx = runErrorT $ runReaderT (runRpcM_ f) ctx
   
instance (IsRemoteError e) => Applicative (RpcM e) where
    pure  = return
    (<*>) = ap

instance (IsRemoteError e) => FreezeIOM RpcContext (Either e) (RpcM e) where
  freeze f = ask >>= liftIO . f
  thaw ctx f = runRpcM f ctx
  cont (Left ex) = throwError ex
  cont (Right v) = return v

instance (IsRemoteError e) => MonadRpc e (RpcM e) where
  rpcGetContext = ask
  rpcLocalContext = local
