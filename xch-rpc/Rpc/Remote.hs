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

{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Rpc.Remote where

import Data.Typeable
import Data.String
import qualified Data.Text.Lazy as TL
import Control.Exception (Exception(..), throwIO)
import Control.Monad

import Rpc.Dispatch
import Rpc.Types
import Rpc.Error
import Rpc.Variables

remote :: Remote a =>  Dispatcher -> Proxy -> String -> a
remote client proxy member = remote_ (\args -> call client (c args)) where 
  c args = RpcCall dest path intf (fromString member) args
  Proxy (RemoteObject dest path) intf = proxy

class Remote a where
    remote_ :: ( [Variant] -> IO (Either RemoteErr [Variant]) ) -> a

class Variables a where
    fromVariants :: [Variant] -> Maybe a


-- handle case if we went return arguments as [Variant]
instance Remote ( IO [Variant] ) where
    remote_ f = h =<< f [] where
        h (Left error) =
            throwIO $ error
        h (Right retv) =
            return $ retv

-- handle case if we want return arguments as (Variables a)
instance (Variables a) => Remote (IO a) where
    remote_ f = h =<< f [] where
        h (Left error) =
            throwIO $ error
        h (Right retv) =
            case fromVariants retv of
              Nothing -> error $ "unexpected number of arguments in reply; " ++ show retv
              Just vs -> return vs


instance (Variable a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\xs -> f (toVariant x:xs))

instance (Variable a, Variable b) => Variables (a,b) where
    fromVariants [a,b] = fromVariant a >>= \a' ->
                         fromVariant b >>= \b' ->
                         return (a',b')
    fromVariants _ = Nothing

instance (Variable a, Variable b, Variable c) => Variables (a,b,c) where
    fromVariants [a,b,c] = fromVariant a >>= \a' ->
                           fromVariant b >>= \b' ->
                           fromVariant c >>= \c' ->
                           return (a',b', c')
    fromVariants _ = Nothing

instance Variables () where
    fromVariants [] = Just ()
    fromVariants _  = Nothing

instance (Variable a) => Variables a where
    fromVariants [a] = fromVariant a
    fromVariants _   = Nothing
