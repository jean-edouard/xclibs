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
module Network.DBus.IntrospectParser where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


{-Type decls-}

data Node = Node Node_Attrs [Node_]
          deriving (Eq,Show)
data Node_Attrs = Node_Attrs
    { nodeName :: (Maybe String)
    } deriving (Eq,Show)
data Node_ = Node_Node Node
           | Node_Interface Interface
           deriving (Eq,Show)
data Interface = Interface Interface_Attrs [Interface_]
               deriving (Eq,Show)
data Interface_Attrs = Interface_Attrs
    { interfaceName :: String
    } deriving (Eq,Show)
data Interface_ = Interface_Method Method
                | Interface_Signal Signal
                | Interface_Property Property
                | Interface_Annotation Annotation
                deriving (Eq,Show)
data Method = Method Method_Attrs [Method_]
            deriving (Eq,Show)
data Method_Attrs = Method_Attrs
    { methodName :: String
    } deriving (Eq,Show)
data Method_ = Method_Arg Arg
             | Method_Annotation Annotation
             deriving (Eq,Show)
data Signal = Signal Signal_Attrs [Signal_]
            deriving (Eq,Show)
data Signal_Attrs = Signal_Attrs
    { signalName :: String
    } deriving (Eq,Show)
data Signal_ = Signal_Arg Arg
             | Signal_Annotation Annotation
             deriving (Eq,Show)
data Arg = Arg
    { argName :: (Maybe String)
    , argType :: String
    , argDirection :: (Defaultable Arg_direction)
    } deriving (Eq,Show)
data Arg_direction = Arg_direction_in  |  Arg_direction_out
                   deriving (Eq,Show)
data Property = Property Property_Attrs [Annotation]
              deriving (Eq,Show)
data Property_Attrs = Property_Attrs
    { propertyName :: String
    , propertyType :: String
    , propertyAccess :: Property_access
    } deriving (Eq,Show)
data Property_access = Property_access_read  |
                       Property_access_write  |  Property_access_readwrite
                     deriving (Eq,Show)
data Annotation = Annotation
    { annotationName :: String
    , annotationValue :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable Node where
    toHType x = Defined "node" [] []
instance XmlContent Node where
    toContents (Node as a) =
        [CElem (Elem "node" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["node"]
        ; interior e $ return (Node (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <node>, "++)
instance XmlAttributes Node_Attrs where
    fromAttrs as =
        Node_Attrs
          { nodeName = possibleA fromAttrToStr "name" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "name" (nodeName v)
        ]

instance HTypeable Node_ where
    toHType x = Defined "node" [] []
instance XmlContent Node_ where
    toContents (Node_Node a) = toContents a
    toContents (Node_Interface a) = toContents a
    parseContents = oneOf
        [ return (Node_Node) `apply` parseContents
        , return (Node_Interface) `apply` parseContents
        ] `adjustErr` ("in <node>, "++)

instance HTypeable Interface where
    toHType x = Defined "interface" [] []
instance XmlContent Interface where
    toContents (Interface as a) =
        [CElem (Elem "interface" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["interface"]
        ; interior e $ return (Interface (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <interface>, "++)
instance XmlAttributes Interface_Attrs where
    fromAttrs as =
        Interface_Attrs
          { interfaceName = definiteA fromAttrToStr "interface" "name" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "name" (interfaceName v)
        ]

instance HTypeable Interface_ where
    toHType x = Defined "interface" [] []
instance XmlContent Interface_ where
    toContents (Interface_Method a) = toContents a
    toContents (Interface_Signal a) = toContents a
    toContents (Interface_Property a) = toContents a
    toContents (Interface_Annotation a) = toContents a
    parseContents = oneOf
        [ return (Interface_Method) `apply` parseContents
        , return (Interface_Signal) `apply` parseContents
        , return (Interface_Property) `apply` parseContents
        , return (Interface_Annotation) `apply` parseContents
        ] `adjustErr` ("in <interface>, "++)

instance HTypeable Method where
    toHType x = Defined "method" [] []
instance XmlContent Method where
    toContents (Method as a) =
        [CElem (Elem "method" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["method"]
        ; interior e $ return (Method (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <method>, "++)
instance XmlAttributes Method_Attrs where
    fromAttrs as =
        Method_Attrs
          { methodName = definiteA fromAttrToStr "method" "name" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "name" (methodName v)
        ]

instance HTypeable Method_ where
    toHType x = Defined "method" [] []
instance XmlContent Method_ where
    toContents (Method_Arg a) = toContents a
    toContents (Method_Annotation a) = toContents a
    parseContents = oneOf
        [ return (Method_Arg) `apply` parseContents
        , return (Method_Annotation) `apply` parseContents
        ] `adjustErr` ("in <method>, "++)

instance HTypeable Signal where
    toHType x = Defined "signal" [] []
instance XmlContent Signal where
    toContents (Signal as a) =
        [CElem (Elem "signal" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["signal"]
        ; interior e $ return (Signal (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <signal>, "++)
instance XmlAttributes Signal_Attrs where
    fromAttrs as =
        Signal_Attrs
          { signalName = definiteA fromAttrToStr "signal" "name" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "name" (signalName v)
        ]

instance HTypeable Signal_ where
    toHType x = Defined "signal" [] []
instance XmlContent Signal_ where
    toContents (Signal_Arg a) = toContents a
    toContents (Signal_Annotation a) = toContents a
    parseContents = oneOf
        [ return (Signal_Arg) `apply` parseContents
        , return (Signal_Annotation) `apply` parseContents
        ] `adjustErr` ("in <signal>, "++)

instance HTypeable Arg where
    toHType x = Defined "arg" [] []
instance XmlContent Arg where
    toContents as =
        [CElem (Elem "arg" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["arg"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <arg>, "++)
instance XmlAttributes Arg where
    fromAttrs as =
        Arg
          { argName = possibleA fromAttrToStr "name" as
          , argType = definiteA fromAttrToStr "arg" "type" as
          , argDirection = defaultA fromAttrToTyp Arg_direction_in "direction" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "name" (argName v)
        , toAttrFrStr "type" (argType v)
        , defaultToAttr toAttrFrTyp "direction" (argDirection v)
        ]

instance XmlAttrType Arg_direction where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "in" = Just Arg_direction_in
            translate "out" = Just Arg_direction_out
            translate _ = Nothing
    toAttrFrTyp n Arg_direction_in = Just (n, str2attr "in")
    toAttrFrTyp n Arg_direction_out = Just (n, str2attr "out")

instance HTypeable Property where
    toHType x = Defined "property" [] []
instance XmlContent Property where
    toContents (Property as a) =
        [CElem (Elem "property" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["property"]
        ; interior e $ return (Property (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <property>, "++)
instance XmlAttributes Property_Attrs where
    fromAttrs as =
        Property_Attrs
          { propertyName = definiteA fromAttrToStr "property" "name" as
          , propertyType = definiteA fromAttrToStr "property" "type" as
          , propertyAccess = definiteA fromAttrToTyp "property" "access" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "name" (propertyName v)
        , toAttrFrStr "type" (propertyType v)
        , toAttrFrTyp "access" (propertyAccess v)
        ]

instance XmlAttrType Property_access where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "read" = Just Property_access_read
            translate "write" = Just Property_access_write
            translate "readwrite" = Just Property_access_readwrite
            translate _ = Nothing
    toAttrFrTyp n Property_access_read = Just (n, str2attr "read")
    toAttrFrTyp n Property_access_write = Just (n, str2attr "write")
    toAttrFrTyp n Property_access_readwrite = Just (n, str2attr "readwrite")

instance HTypeable Annotation where
    toHType x = Defined "annotation" [] []
instance XmlContent Annotation where
    toContents as =
        [CElem (Elem "annotation" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["annotation"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <annotation>, "++)
instance XmlAttributes Annotation where
    fromAttrs as =
        Annotation
          { annotationName = definiteA fromAttrToStr "annotation" "name" as
          , annotationValue = definiteA fromAttrToStr "annotation" "value" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "name" (annotationName v)
        , toAttrFrStr "value" (annotationValue v)
        ]



{-Done-}
