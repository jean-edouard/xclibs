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

module Main where

import Control.Monad
import Data.Maybe
import qualified Network.WebSocket as W
import Network.Socket
import qualified Network.Socket.ByteString as NB
import qualified Data.ByteString.Lazy.UTF8 as UTF8

frames s =
  do f <- W.recvFrame s 0x10000
     putStrLn (show f)
     when (isJust f) $
       frames s
     
talk (s, addr) =
  do putStrLn $ "new connection: " ++ show addr
     webs <- W.create (NB.recv s) (NB.send s)
     W.handshake webs
     putStrLn $ "handshake done"
     -- W.sendFrame webs (W.Text, UTF8.fromString "{\"x\": 0, \"y\": 0, \"width\":640, \"height\": 480}")
     frames webs
     W.shutdown webs
     putStrLn $ "shutdown done"
     sClose s
     
server =
  do s <- socket AF_INET Stream defaultProtocol
     setSocketOption s ReuseAddr 1
     bindSocket s (SockAddrInet 8181 iNADDR_ANY)
     listen s 1
     talk =<< accept s
     sClose s
     
main = server
