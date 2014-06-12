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

{-# LANGUAGE TypeSynonymInstances,ScopedTypeVariables,FlexibleInstances,UndecidableInstances,OverlappingInstances,PatternGuards,FlexibleContexts #-}

module Rpc.Variables
       ( DBusTypeable (..)
       , Variable (..)
       ) where

import Control.Applicative
import Data.Maybe
import Data.Int
import Data.Word
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Map as M
import Data.Map (Map)
import Rpc.Types

import qualified Network.DBus as D
--
-- instances for easier mapping complex haskell types e.g. maps and lists to dbus counterparts
--

-- we need a class to infer dbus type from haskell one
class DBusTypeable a where
    dbusType :: a -> D.SignatureElem
    -- required for specialsed instance for [Char]
    dbusListType :: [a] -> D.SignatureElem
    dbusListType _
      = let elemT = dbusType (undefined :: a)
        in D.SigArray elemT

-- | convert between haskell & dbus types
class Variable a where
  fromVariant :: DBusValue -> Maybe a
  toVariant   :: a -> DBusValue

  -- required to give specialised instance for [Word8], [Char]
  listFromVariant :: DBusTypeable a => DBusValue -> Maybe [a]
  listFromVariant = listFromVariant_
  
  listToVariant :: DBusTypeable a => [a] -> DBusValue
  listToVariant = listToVariant_
  
listToVariant_ :: forall a. (DBusTypeable a, Variable a) => [a] -> DBusValue
listToVariant_ xs
  = let elemT = dbusType (undefined :: a) in 
  D.DBusArray elemT $ map toVariant xs

listFromVariant_ :: forall a. (DBusTypeable a, Variable a) => DBusValue -> Maybe [a]
listFromVariant_ = f where 
  f (D.DBusArray t xs) | t == dbusType (undefined :: a)
      = mapM fromVariant xs
  f _ = Nothing

-- map standard types to dbus types
instance DBusTypeable Bool where
    dbusType _ = D.SigBool

instance DBusTypeable Word8 where
    dbusType _ = D.SigByte

instance DBusTypeable Int16 where
    dbusType _ = D.SigInt16

instance DBusTypeable Int32 where
    dbusType _ = D.SigInt32

instance DBusTypeable Int64 where
    dbusType _ = D.SigInt64

instance DBusTypeable Word16 where
    dbusType _ = D.SigUInt16

instance DBusTypeable Word32 where
    dbusType _ = D.SigUInt32

instance DBusTypeable Word64 where
    dbusType _ = D.SigUInt64

instance DBusTypeable Char where
    dbusType _ = D.SigString
    dbusListType _ = D.SigString

instance DBusTypeable TL.Text where
    dbusType _ = D.SigString
    
instance DBusTypeable Double where
    dbusType _ = D.SigDouble

instance DBusTypeable D.DBusValue where
    dbusType _ = D.SigVariant

instance DBusTypeable ObjectPath where
    dbusType _ = D.SigObjectPath

instance DBusTypeable D.SignatureElem where
    dbusType _ = D.SigSignature
    dbusListType _ = D.SigSignature

instance (DBusTypeable a) => DBusTypeable [a] where
    dbusType = dbusListType

instance (Ord k, DBusTypeable k, DBusTypeable v) => DBusTypeable (Map k v) where
    dbusType _ = let keyT  = dbusType (undefined :: k)
                     elemT = dbusType (undefined :: v)
                 in
                   D.SigArray (D.SigDict keyT elemT)

-- | some mind numbing Variable instances

instance Variable TL.Text where
  toVariant = D.DBusString . D.PackedString . B.concat . BL.toChunks . TLE.encodeUtf8
  fromVariant (D.DBusString (D.PackedString x)) = Just (TLE.decodeUtf8 . BL.fromChunks . (:[]) $ x)
  fromVariant _ = Nothing

instance Variable B.ByteString where
  toVariant = D.DBusByteArray
  fromVariant (D.DBusByteArray b) = Just b
  fromVariant (D.DBusArray D.SigByte xs) = B.pack <$> mapM fromVariant xs
  fromVariant _ = Nothing
  
instance Variable Bool where
  toVariant = D.DBusBoolean
  fromVariant (D.DBusBoolean x) = Just x
  fromVariant _ = Nothing
  
-- marshall char as dbus string
instance Variable Char where
  toVariant c = listToVariant [c]
  fromVariant v = case listFromVariant v of
    Just (x:_) -> Just x
    _ -> Nothing
  
  -- handle [Char] aka String
  listToVariant = D.DBusString . D.PackedString . BUTF8.fromString
  listFromVariant (D.DBusString (D.PackedString x)) = Just $ BUTF8.toString x
  listFromVariant _ = Nothing

instance Variable Word8 where
  toVariant = D.DBusByte
  fromVariant (D.DBusByte x) = Just x
  fromVariant _ = Nothing
  
  listToVariant = D.DBusByteArray . B.pack
  listFromVariant (D.DBusArray D.SigByte xs) = mapM fromVariant xs
  listFromVariant (D.DBusByteArray xs) = Just $ B.unpack xs
  listFromVariant _ = Nothing
  
instance Variable Word16 where
  toVariant = D.DBusUInt16
  fromVariant (D.DBusUInt16 x) = Just x
  fromVariant _ = Nothing

instance Variable Word32 where
  toVariant = D.DBusUInt32
  fromVariant (D.DBusUInt32 x) = Just x
  fromVariant _ = Nothing

instance Variable Word64 where
  toVariant = D.DBusUInt64
  fromVariant (D.DBusUInt64 x) = Just x
  fromVariant _ = Nothing

instance Variable Int16 where
  toVariant = D.DBusInt16
  fromVariant (D.DBusInt16 x) = Just x
  fromVariant _ = Nothing

instance Variable Int32 where
  toVariant = D.DBusInt32
  fromVariant (D.DBusInt32 x) = Just x
  fromVariant _ = Nothing

instance Variable Int64 where
  toVariant = D.DBusInt64
  fromVariant (D.DBusInt64 x) = Just x
  fromVariant _ = Nothing

instance Variable Double where
  toVariant = D.DBusDouble
  fromVariant (D.DBusDouble x) = Just x
  fromVariant _ = Nothing

instance Variable D.DBusValue where
  toVariant = D.DBusVariant
  fromVariant (D.DBusVariant x) = Just x
  fromVariant _ = Nothing
  
instance Variable ObjectPath where
  fromVariant (D.DBusObjectPath p) = Just . mkObjectPath_ . TL.pack $ show p
  fromVariant _ = Nothing
  toVariant p = D.DBusObjectPath . fromString . TL.unpack . strObjectPath $ p
  
instance Variable D.SignatureElem where
  toVariant c = listToVariant [c]
  fromVariant v = case listFromVariant v of
    Just (x:_) -> Just x
    _ -> Nothing

  listToVariant = D.DBusSignature
  listFromVariant (D.DBusSignature x) = Just x
  listFromVariant _ = Nothing
  
-- these are equivalent to strings actually, instances for convenience
instance Variable BusName where
  fromVariant x = mkBusName_  <$> fromVariant x
  toVariant = toVariant . strBusName
instance Variable InterfaceName where
  fromVariant x = mkInterfaceName_ <$> fromVariant x
  toVariant = toVariant . strInterfaceName
instance Variable MemberName where
  fromVariant x = mkMemberName_ <$> fromVariant x
  toVariant = toVariant . strMemberName

-- list of variables we can infer dbus type for, is a variable
instance (DBusTypeable a, Variable a) => Variable [a] where
  toVariant   = listToVariant
  fromVariant = listFromVariant
  
-- map is a variable if we can infer types of keys and elements, and they're variables
instance (Ord k, DBusTypeable k, Variable k, DBusTypeable v, Variable v) => Variable (Map k v) where
    toVariant m =
        let keyT      = dbusType (undefined :: k)
            elemT     = dbusType (undefined :: v)
            elems     = map (\(k,v) -> D.DBusDict (toVariant k) (toVariant v)) $ M.toList m
        in D.DBusArray (D.SigDict keyT elemT) elems

    fromVariant (D.DBusArray (D.SigDict keyT elemT) xs)
      |   keyT  == dbusType (undefined :: k)
        , elemT == dbusType (undefined :: v)
        = M.fromList <$> sequence (map mk xs)
            where mk (D.DBusDict k v) = (,) <$> fromVariant k <*> fromVariant v
                  mk _ = Nothing
    fromVariant _ = Nothing
