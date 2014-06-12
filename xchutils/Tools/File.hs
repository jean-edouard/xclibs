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

-- Some utility file functions
module Tools.File (
              maybeGetContents
            , filesInDir
            , getDirectoryContents_nonDotted
            , fileSha1Sum
            , fileSha256Sum
            , readFileStrict
            , module Tools.FileC
            ) where

import Control.Monad
import Control.Applicative
import System.Directory
import System.IO
import System.FilePath.Posix
import Tools.FileC
import Tools.Process
import Tools.Misc

maybeGetContents :: FilePath -> IO (Maybe String)
maybeGetContents p = doesFileExist p >>= maybeRead
          where maybeRead False = return Nothing
                maybeRead True  = readFileStrict p >>= return . Just

readFileStrict :: FilePath -> IO String
readFileStrict p = do
  contents <- readFile p
  length contents `seq` return contents

filesInDir :: FilePath -> IO [FilePath]
filesInDir p = map (p </>) <$> getDirectoryContents_nonDotted p

dotted :: FilePath -> Bool
dotted "." = True
dotted ".." = True
dotted _ = False

getDirectoryContents_nonDotted :: FilePath -> IO [FilePath]
getDirectoryContents_nonDotted = fmap (filter $ not . dotted) . getDirectoryContents

fileSha1Sum :: FilePath -> IO Integer
fileSha1Sum path = do
  (sumStr:_) <- reverse . words <$> readProcessOrDie "openssl" ["dgst", "-sha1", path] ""
  return $ read ("0x" ++ sumStr)

fileSha256Sum :: FilePath -> IO Integer
fileSha256Sum path = do
  (sumStr:_) <- reverse . words <$> readProcessOrDie "openssl" ["dgst", "-sha256", path] ""
  return $ read ("0x" ++ sumStr)
