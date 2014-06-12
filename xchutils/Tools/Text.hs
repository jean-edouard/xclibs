--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module Tools.Text where

import qualified Data.Text as T

-- chop line ending characters off
chomp :: String -> String
chomp s = case reverse s of
            ('\n' : '\r' : xs) -> reverse xs
            ('\n' : xs)        -> reverse xs
            _                  -> s

replace :: String -> String -> String -> String
replace pat repl txt =
    T.unpack $ T.replace (T.pack pat) (T.pack repl) (T.pack txt)

-- Strip a string from whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

maybeRead :: (Read a) => String -> Maybe a
maybeRead str = case reads str of
                  (v,_):_ -> Just v
                  _ -> Nothing
