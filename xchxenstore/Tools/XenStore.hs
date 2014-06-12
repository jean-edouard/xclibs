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

module Tools.XenStore ( module Tools.XenStoreC
                      , xsChmod ) where

import Prelude hiding (read)
import Control.Applicative
import Control.Monad
import Control.Exception
import System.Process
import Tools.XenStoreC

spawn :: FilePath -> [String] -> IO (Maybe String)
spawn cmd args =
    con `liftM` (try $ readProcess cmd args [])
  where
    con :: Either IOError String -> Maybe String
    con (Left  e) = Nothing
    con (Right s) = Just s

xsChmod :: String -> String -> IO ()
xsChmod path perm = spawn "xenstore-chmod" [path, perm] >> return ()


