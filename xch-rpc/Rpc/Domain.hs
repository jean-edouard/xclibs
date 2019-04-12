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

module Rpc.Domain where

import Control.Applicative
import Control.Monad.Trans
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as M

import Rpc.Monad
import Rpc.Log
import Rpc.Dispatch
import qualified Rpc.DBusArgo as A
import qualified Control.Exception as E

-- rpc on another domain's bus
-- this even allows funky things like exporting service on another domain bus but with processing in dom0
rpcWithDomain :: MonadRpc e m => Int -> m a -> m a
rpcWithDomain domain f =
    rpcGetContext >>= \context ->
    liftIO (rpcCacheDomainBus domain context) >>= \client' ->
        rpcLocalContext (\c -> c{ client = client' } ) f


rpcCacheDomainBus :: Int -> RpcContext -> IO Dispatcher
rpcCacheDomainBus domid context
  = modifyMVar (domUClients context) $ \clients -> 
      case M.lookup domid clients of
        Nothing -> connected clients =<< rpcTryConnectDomainBus domid 120
        Just b  -> return (clients, b)
  where
    connected clients Nothing =
      do warn $ "giving up on accessing domain's " ++ show domid ++ " system bus."
         ioError . userError $ "exceeded maximum attempt count trying to connect to domain's system bus"
    connected clients (Just b) =
      return (M.insert domid b clients, b)

rpcTryConnectDomainBus :: Int -> Int -> IO (Maybe Dispatcher)
rpcTryConnectDomainBus domid 0 = return Nothing
rpcTryConnectDomainBus domid timeout
  = ( Just <$> get ) `E.catch` retry
  where
    get = A.domainSystemBus domid >>= connectBus >>= return . fst
    retry :: E.SomeException -> IO (Maybe Dispatcher)
    retry e = do warn $ "domain's " ++ show domid ++ " sytem bus is unresponsive: " ++ show e ++ ", retrying.."
                 threadDelay (10^6)
                 rpcTryConnectDomainBus domid (timeout-1)
