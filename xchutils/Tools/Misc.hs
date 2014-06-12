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

module Tools.Misc (
               split
             , splitBy
             , void
             , differenceList
             , schedule, reschedule, ScheduledTask
             ) where

import qualified Data.Text as T
import Control.Applicative
import Control.Concurrent
import Data.Time
import System.Process
import System.IO
import System.Exit

void :: Functor f => f a -> f ()
void = fmap (const ())

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split = splitBy . (==)

-- separate a list into sub-lists on separator elements
--
-- splitBy (=="\n") == Prelude.lines
-- I.e. splitBy (=="\n") "a\n\nb\nc" == ["a","","b","c"]
--
-- Adapted from Prelude.lines.  It's a bit convoluted to work around a
-- GHC bug.  See the implementation of Prelude.lines for an
-- explanation.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy pred = split
  where split s =  cons (case break pred s of
                            (l, s') -> (l, case s' of
                                           [] -> []
                                           _:s'' -> split s''))
        cons ~(h, t) =  h:t

differenceList :: (Eq a) => [a] -> [a] -> [a]
differenceList xs ys = [x | x <- xs, not (x `elem` ys)]

data ScheduledTask = ScheduledTask { taskDone :: MVar Bool
                                   , taskAction :: IO ()
                                   , taskRunner :: MVar ThreadId }


schedule :: DiffTime -> IO () -> IO ScheduledTask
schedule dt action =
    do t0 <- getCurrentTime
       let t1 = addUTCTime (realToFrac dt) t0
       done_mv <- newMVar False
       runner_id <- forkIO $ startTaskRunner action t1 done_mv
       runner_id_mv <- newMVar runner_id
       return ScheduledTask {
                    taskDone = done_mv
                  , taskAction = action
                  , taskRunner = runner_id_mv
                  }

reschedule :: DiffTime -> ScheduledTask -> IO ()
reschedule dt task =
    do t0 <- getCurrentTime
       let t1 = addUTCTime (realToFrac dt) t0
       modifyMVar_ (taskRunner task) $ \thread_id -> do
         killThread thread_id
         forkIO $ startTaskRunner (taskAction task) t1 (taskDone task)

startTaskRunner :: IO () -> UTCTime -> MVar Bool -> IO ()
startTaskRunner action run_time done =
    do t0 <- getCurrentTime
       let dt        = diffUTCTime run_time t0
           dt_secs   = realToFrac dt :: Double
           dt_micros = round (dt_secs * 10^6)
       if dt_micros <= 0
          then run
          else threadDelay dt_micros >> startTaskRunner action run_time done
    where
      run = modifyMVar_ done f
            where f False = action >> return True
                  f _ = return True

