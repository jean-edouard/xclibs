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

{-# LANGUAGE DeriveDataTypeable #-}
module Rpc.Error where

import Data.Typeable
import qualified Control.Exception as E
import Rpc.Types

data RemoteErr = RemoteErr { remoteErrorName :: ErrorName
                           , remoteErrorBody :: [DBusValue] }
                 deriving (Eq,Show,Typeable)

instance E.Exception RemoteErr

--
-- error
--
class (Show e) => IsRemoteError e where
    fromRemoteErr :: RpcCall -> RemoteErr -> e
    toRemoteErr :: e -> Maybe RemoteErr


