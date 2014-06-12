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

{-# LANGUAGE OverloadedStrings #-}

module Network.DBus.Introspect
       (
         DocString
       , Object (..)
       , Interface (..)
       , Method (..)
       , Property (..)
       , Signal (..)
       , Arg (..)
       , Access (..)
       , fromXml
-- TODO:
--       , toXML
       ) where

import Network.DBus
import Network.DBus.Actions (unserializeSignature)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as C
import Data.Text.Lazy (Text)
import Data.String
import Data.Maybe
import Text.XML.HaXml
import qualified Text.XML.HaXml.XmlContent as XC
import qualified Network.DBus.IntrospectParser as P
import Control.Monad

type DocString = TL.Text

data Object
   = Object
     {
       objDocString :: Maybe DocString
     , objPath :: ObjectPath
     , objInterfaces :: [Interface]
     }

data Interface
   = Interface
     {
       intfDocString :: Maybe DocString
     , intfName :: Text
     , intfMethods :: [Method]
     , intfProperties :: [Property]
     , intfSignals :: [Signal]
     }

data Method
   = Method
     {
       methDocString :: Maybe DocString
     , methName :: Text
     , methInArgs :: [Arg]
     , methOutArgs :: [Arg]
     }

data Property
   = Property
     {
       propDocString :: Maybe DocString
     , propName :: Text
     , propType :: SignatureElem
     , propAccess :: Access
     }

data Signal
   = Signal
     {
       sigDocString :: Maybe DocString
     , sigName :: Text
     , sigArgs :: [Arg]
     }

data Arg
   = Arg
     {
       argDocString :: Maybe DocString
     , argName :: Maybe Text
     , argType :: SignatureElem
     }

data Access
   = Read | Write | ReadWrite

fromXml :: TL.Text -> Either String Object
fromXml = fmap fromPNode . XC.fromXml . xmlParse [] . TL.unpack

fromPNode :: P.Node -> Object
fromPNode (P.Node attrs child)
  = Object
    { objDocString = Nothing
    , objPath = fromString (fromMaybe "/" $ P.nodeName attrs)
    , objInterfaces = catMaybes . map iface $ child }
  where
    iface (P.Node_Interface i) = Just $ fromPInterface i
    iface _ = Nothing

fromPInterface :: P.Interface -> Interface
fromPInterface (P.Interface attrs child)
  = Interface
    { intfDocString = Nothing
    , intfName = fromString (P.interfaceName attrs)
    , intfMethods = catMaybes . map meth $ child
    , intfProperties = catMaybes . map prop $ child
    , intfSignals = catMaybes . map sig $ child }
  where
    meth (P.Interface_Method v) = Just $ fromPMethod v
    meth _ = Nothing
    prop (P.Interface_Property v) = Just $ fromPProperty v
    prop _ = Nothing
    sig (P.Interface_Signal v) = Just $ fromPSignal v
    sig _ = Nothing

argdir a = case P.argDirection a of
  XC.Default    v -> v
  XC.NonDefault v -> v

fromPMethod :: P.Method -> Method
fromPMethod (P.Method attrs child)
  = Method
    { methDocString = Nothing
    , methName = fromString (P.methodName attrs)
    , methInArgs = catMaybes . map argin $ child
    , methOutArgs = catMaybes . map argout $ child }
  where
    argin (P.Method_Arg arg)
      | argdir arg == P.Arg_direction_in = Just $ fromPArg arg
    argin _ = Nothing
    argout (P.Method_Arg arg)
      | argdir arg == P.Arg_direction_out = Just $ fromPArg arg
    argout _ = Nothing

fromPProperty :: P.Property -> Property
fromPProperty (P.Property attrs _)
  = Property
    { propDocString = Nothing
    , propName = fromString (P.propertyName attrs)
    , propType = head' $ sigFromStr (P.propertyType attrs)
    , propAccess = access (P.propertyAccess attrs)
    }
  where
    access P.Property_access_read = Read
    access P.Property_access_write = Write
    access P.Property_access_readwrite = ReadWrite
    head' (v:_) = v
    head' _ = SigVariant --fixme

fromPSignal :: P.Signal -> Signal
fromPSignal (P.Signal attrs child)
  = Signal
    { sigDocString = Nothing
    , sigName = fromString (P.signalName attrs)
    , sigArgs = catMaybes . map argin $ child }
  where
    argin (P.Signal_Arg arg)
      | argdir arg == P.Arg_direction_in = Just $ fromPArg arg
    argin _ = Nothing

fromPArg :: P.Arg -> Arg
fromPArg a
  = Arg
    { argDocString = Nothing
    , argName = fromString `fmap` (P.argName a)
    , argType = head' $ sigFromStr (P.argType a)
    }
  where
    head' (v:_) = v
    head' _ = SigVariant --fixme

sigFromStr :: String -> Signature
sigFromStr = either (const []) id . unserializeSignature . C.pack
