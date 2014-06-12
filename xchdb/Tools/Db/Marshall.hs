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

{-# LANGUAGE TypeSynonymInstances,OverlappingInstances,TypeOperators,FlexibleInstances,ViewPatterns,TupleSections #-}

-- Higher level interface for database access
-- which allows to read / write any types implementing Marshall class
module Tools.Db.Marshall (
            Marshall
          , EnumMarshall(..)
          , dbPathSplit
          , dbRead
          , dbReadWithDefault
          , dbMaybeRead
          , dbMaybeWrite
          , dbWrite
          , dbReadEnum
          , dbWriteEnum
          , enumMarshall
          , enumMarshallReverse
          , enumMarshallReverse_
          ) where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Int
import Data.String
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Control.Arrow
import Tools.Misc
import Tools.Text
import Tools.IfM
import Tools.Db.Core
import Rpc.Core

type Path = String

pathA </> pathB = pathA ++ "/" ++ pathB

--
-- Class for types which can be marshalled from / to database
--
class (Eq p) => Marshall p where
    dbRead  :: (MonadRpc e m) => Path -> m p
    dbWrite :: (MonadRpc e m) => Path -> p -> m ()

-- String can be easily parshalled
instance Marshall String where
    dbRead = dbReadStr
    dbWrite = dbWriteStr

-- Integer can do it too
instance Marshall Int where
    dbRead = liftM Prelude.read . dbReadStr
    dbWrite x = dbWriteStr x . show

instance Marshall Int32 where
    dbRead = liftM Prelude.read . dbReadStr
    dbWrite x = dbWriteStr x . show

instance Marshall Double where
    dbRead = liftM Prelude.read . dbReadStr
    dbWrite x = dbWriteStr x . show

-- Boolean can do it too
instance Marshall Bool where
    dbRead x = liftM fromS . dbReadStr $ x
              where
                -- People write booleans in all kind of ways..
                fromS "true"  = True
                fromS "yes"   = True
                fromS "1"     = True
                fromS "false" = False
                fromS "no"    = False
                fromS "0"     = False
                fromS s       = error $ "unexpected boolean text representation: " ++ s ++ " while reading " ++ x

    dbWrite x = dbWriteStr x . toS
              where
                -- But we write them only in one true way
                toS True  = "true"
                toS False = "false"

-- List of marshalled types is marshalled as well
instance (Marshall a) => Marshall [a] where
    dbRead         = mapM dbRead <=< dbListPaths
    dbWrite x vs   = dbRm x        >>  zipWithM_ dbWrite paths vs
                     where paths        = map ((x </>) . show) [0..]

-- maps of marshalable types are marshalable -- but we don't want to 'show' a string.
instance (Marshall v) => Marshall (M.Map String v) where
    dbRead x    = M.fromList <$> (readAll =<< dbList x)
      where readAll ids = zip ids <$> mapM (dbRead . (x </>)) ids

    dbWrite x (M.map Just . M.mapKeys show -> new) = do
        old <- M.fromList . map (,Nothing) <$> dbList x
        -- F.sequence_ processes Map in ascending orders of keys.
        F.sequence_ . M.mapWithKey (dbMaybeWrite . (x </>)) $ M.unionWith mplus new old

-- maps of marshalable types are marshalable
instance (Ord k, Show k, IsString k, Marshall k, Marshall v) => Marshall (M.Map k v) where
    dbRead x    = M.fromList <$> (readAll . map fromString =<< dbList x)
      where readAll ids = zip ids <$> mapM (dbRead . (x </>) . show) ids

    dbWrite x (M.map Just . M.mapKeys show -> new) = do
        old <- M.fromList . map (,Nothing) <$> dbList x
        -- F.sequence_ processes Map in ascending orders of keys.
        F.sequence_ . M.mapWithKey (dbMaybeWrite . (x </>)) $ M.unionWith mplus new old

dbPathSplit :: String -> [String]
dbPathSplit = filter meaningful . split '/' where
  meaningful x = let x' = strip x in not (null x')

-- Read database node if it exists, otherwise return nothing
dbMaybeRead :: (Marshall a, MonadRpc e m) => Path -> m (Maybe a)
dbMaybeRead p = ifM (dbExists p) (Just <$> dbRead p)
                                 (return Nothing)

dbReadWithDefault :: (Marshall a, MonadRpc e m) => a -> Path -> m a
dbReadWithDefault d p = fromMaybe d <$> dbMaybeRead p

swap (a, b) = (b, a)

class Eq a => EnumMarshall a where
    enumMarshallMap        :: [(a, String)]
    enumMarshallMapReverse :: [(String, a)]
    enumMarshallMap         = Data.List.map swap enumMarshallMapReverse
    enumMarshallMapReverse  = Data.List.map swap enumMarshallMap

enumLookup :: Eq a => [(a, b)] -> a -> b
enumLookup m v = fromMaybe (error "unexpected enumeration value") (lookup v m)

enumMarshall :: EnumMarshall a => a -> String
enumMarshall = enumLookup enumMarshallMap

enumMarshallReverse_ :: EnumMarshall a => String -> a
enumMarshallReverse_ = enumLookup enumMarshallMapReverse

enumMarshallReverse :: EnumMarshall a => String -> Maybe a
enumMarshallReverse k = lookup k enumMarshallMapReverse

dbReadEnum:: (MonadRpc e m, EnumMarshall a) => String -> m a
dbReadEnum = fmap enumMarshallReverse_ . dbReadStr

dbWriteEnum :: (MonadRpc e m, EnumMarshall a) => String -> a -> m ()
dbWriteEnum x = dbWriteStr x . enumMarshall

dbMaybeWrite :: (MonadRpc e m, Marshall a) => String -> Maybe a -> m ()
dbMaybeWrite p v = case v of
    Nothing -> dbRm p
    Just v -> dbWrite p v

