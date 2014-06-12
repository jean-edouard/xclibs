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

{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
-- Simple interface to database daemon
module Tools.Db.Core
          ( dbReadStr
          , dbWriteStr
          , dbExists
          , dbList
          , dbListPaths
          , dbRm
          , dbInject
          , dbDump
          , dbDumpStr
          , dbMv
          ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Text.JSON
import Tools.IfM

import Rpc.Core
import Rpc.Autogen.DbClient

type Path = String

service = "com.citrix.xenclient.db"
objpath = "/"
call f  = f service objpath

-- Read from database. Empty string when DB node does not exist
dbReadStr :: (MonadRpc e m) => Path -> m String
dbReadStr = call comCitrixXenclientDbRead

-- Write to database
dbWriteStr :: (MonadRpc e m) => Path -> String -> m ()
dbWriteStr = call comCitrixXenclientDbWrite

-- List child nodes of a given node
dbList :: (MonadRpc e m) => Path -> m [String]
dbList = call comCitrixXenclientDbList

-- Check if a path exists
dbExists :: (MonadRpc e m) => Path -> m Bool
dbExists = call comCitrixXenclientDbExists

-- List child paths of a given node
dbListPaths :: (MonadRpc e m) => Path -> m [Path]
dbListPaths path = map prefixPath <$> dbList path
  where prefixPath y = path ++ "/" ++ y

-- Remove a node with subnodes
dbRm :: (MonadRpc e m) => Path -> m ()
dbRm = call comCitrixXenclientDbRm

dbInject :: (MonadRpc e m) => Path -> String -> m ()
dbInject = call comCitrixXenclientDbInject

dbDump :: (MonadRpc e m) => Path -> m (Maybe JSValue)
dbDump = liftM ((resultToMaybe . decodeStrict) =<<) . dbDumpStr where

resultToMaybe (Error _) = Nothing
resultToMaybe (Ok json) = Just json

dbDumpStr :: (MonadRpc e m) => Path -> m (Maybe String)
dbDumpStr path = ifM (dbExists path)
                 (Just <$> call comCitrixXenclientDbDump path)
                 (return Nothing)

dbMv :: (MonadRpc e m) => Path -> Path -> m ()
dbMv from to = do
  contents <- dbDumpStr from
  case contents of
    Just contents -> dbInject to contents >> dbRm from
    Nothing -> return ()
