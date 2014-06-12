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

module Tools.Periodic (
                          Context
                        , empty
                        , periodically
                        , once
                        , service
                        ) where

import Data.List
import Control.Concurrent

data ActionType = Periodic Int | Once

data Event = Event { atyp  :: ActionType
                   , waitT :: Int
                   , run   :: IO () }

newtype Context = Context [Event]

periodT :: ActionType -> Int
periodT (Periodic t) = t
periodT _            = 0

initEvent :: Int -> ActionType -> IO () -> Event
initEvent waitt typ f =
    Event typ waitt f

timeline :: Context -> [Event]
timeline (Context events) =
    sortBy cmp events
  where
    cmp a b = compare (waitT a) (waitT b)

queue :: Int -> Event -> Context -> Context
queue when e (Context es) =
    Context $ e { waitT = when } : es

subtractTime :: Int -> Event -> Event
subtractTime dt e =
    e { waitT = waitT e - dt }

runNext :: Context -> IO Context
runNext context =
    case timeline context of
      []     -> return $ Context []
      (x:xs) -> do
        let dt  = max (waitT x) 0
            xs' = map (subtractTime dt) xs
        threadDelay (dt*1000)
        run x
        case atyp x of
          Periodic t -> return $ queue t x (Context xs')
          Once       -> return $ Context xs'

-- Empty context with no actions
empty :: Context
empty = Context []

-- Define action to be performed periodically
periodically :: Context -> Int -> IO () -> Context
periodically context period f =
    queue 0 (initEvent 0 (Periodic period) f) context

-- Define action to be performed once (say when)
once :: Context -> Int -> IO () -> Context
once context when f =
    queue when (initEvent when Once f) context

-- Service action queue
service :: Context -> IO ()
service context = do
  c' <- runNext context
  case c' of
    Context [] -> return ()
    _          -> service c'
