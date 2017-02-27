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

{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Rpc.DBusV4V ( 
    domainSystemBus
  , remoteDomainBus
  ) where

import qualified Control.Exception as E
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Data.Set as Set
import Data.Maybe
import Data.String
import Network.Socket (SocketType (..))
import System.Posix.Types
import System.Posix.IO
import Rpc.Log

import qualified Data.ByteString as B
import qualified Network.DBus as D
import qualified Network.DBus.Actions as D

#ifdef USE_V4V
import qualified Tools.V4V as V
#endif

domainSystemBus :: Int -> IO D.DBusContext
remoteDomainBus :: Int -> Int -> IO D.DBusContext

#ifndef USE_V4V
domainSystemBus = error "dbus v4v unsupported"
remoteDomainBus = error "dbus v4v unsupported"
#else

domainSystemBus domain = remoteDomainBus domain 5555
remoteDomainBus domain v4vPort = do
  let addr = V.Addr v4vPort domain
  fd <- connect addr
  info $ "connected to domain's " ++ show domain ++ " system bus proxy"
  D.contextNewWith (v4vTransport fd)
  where
    connect addr = V.socket Stream >>= \f ->
      -- be careful to close fd on connect error..
                  ( do setFdOption f NonBlockingRead False
                       V.connect f addr
                       setFdOption f NonBlockingRead True
                       return f )
                  `E.catch` connect_error f
                      where connect_error f (err::E.SomeException) = V.close f >> E.throw err
    v4vTransport fd
      = D.DBusTransport { D.transportPut  = send fd
                        , D.transportGet  = recv fd
                        , D.transportClose = close fd }
    send  fd buf       = do sent <- V.send fd buf 0
                            if sent < (B.length buf)
                               then send fd (B.drop sent buf)
                               else return ()
      -- seems to be needed because stupid dbus bindings do recv 0 and v4v blocks on that ?
    recv  fd 0         = return B.empty
    recv  fd sz        = recv_aux fd (fromIntegral sz)
    recv_aux fd sz     = do chunk <- V.recv fd (fromIntegral $ sz) 0
                            case B.length chunk of
                                0           -> return chunk
                                l | l >= sz -> return chunk
                                _           -> B.append chunk <$> recv_aux fd (sz - B.length chunk)

    close fd        = V.closeSafe fd
        
#endif
