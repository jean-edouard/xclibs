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
module Rpc.Intro
       ( introXml
       ) where

--TODO: signals in introspection xml

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as C
import Rpc.Types
import qualified Network.DBus as D
import qualified Network.DBus.Actions as D

(<+>) = TL.append
q = "\""
quote x = q <+> x <+> q

introXml :: ObjectPath -> [RpcInterface m] -> Text
introXml p ifaces =
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n"
  <+>
  "<node name=" <+> quote (strObjectPath p) <+> " xmlns:tp=\"http://telepathy.freedesktop.org/wiki/DbusSpec#extensions-v0\">\n"
  <+>
  TL.concat (map interfaceXml ifaces)
  <+> "</node>\n"
  
interfaceXml :: RpcInterface m -> Text
interfaceXml (RpcInterface name methods properties)
   = "<interface name=" <+> quote (strInterfaceName name) <+> ">\n"
 <+> TL.concat (map propertyXml properties)
 <+> TL.concat (map methodXml methods)
 <+> "</interface>\n"

methodXml :: RpcMethod m -> Text
methodXml (RpcMethod pin pout name _)
   = "<method name=" <+> quote (strMemberName name) <+> ">\n"
 <+> TL.concat (map (argXml "in" ) pin)
 <+> TL.concat (map (argXml "out") pout)
 <+> "</method>\n"

propertyXml :: RpcProperty -> Text
propertyXml (RpcProperty typ name access)
   = "<property name=" <+> quote (strMemberName name) <+> " type=" <+> quote (typeStr typ) <+> " access=" <+> quote accessS <+> "/>\n"
  where
    accessS = case access of
      Read      -> "read"
      Write     -> "write"
      ReadWrite -> "readwrite"
      
argXml :: Text -> RpcParam -> Text
argXml dir (RpcParam name typ)
   = "<arg name=" <+> quote name <+> " type=" <+> quote typS <+> " direction=" <+> quote dir <+> "/>\n"
  where
    typS = typeStr typ
    
typeStr typ =
  TL.pack $ C.unpack $ D.serializeSignature [typ]
