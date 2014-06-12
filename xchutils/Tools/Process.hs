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

{-# LANGUAGE ViewPatterns #-}
module Tools.Process (spawnShell
                     ,spawnShell'
                     ,safeSpawnShell
                     ,getCurrentRunLevel
                     ,readProcess
                     ,readProcessOrDie
                     ,readProcessOrDieWithEnv
                     ,readProcess_closeFds
                     ,readProcessWithExitCode_closeFds
                     ,runInteractiveCommand_closeFds
                     )
       where

import Data.List (intercalate)
import Data.Maybe
import System.Process
import System.Process.Internals (runGenProcess_)
import System.Exit
import System.Environment
import System.IO
import System.IO.Error
import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E
import GHC.IO.Exception (IOErrorType(..))

-- ToDo: Consider actually parsing the output "unknown" instead of just relying on the ExitFailure?
getCurrentRunLevel :: IO (Maybe Int)
getCurrentRunLevel = (liftM.liftM) parse $ spawnShell' "runlevel" where
  parse (words -> [prevStr, read -> current]) = current

-- Execute shell command and wait for its output, return empty string in case of exit failure
spawnShell :: String -> IO String
spawnShell cmd =
    spawnShell' cmd >>= f where f Nothing  = return ""
                                f (Just s) = return s

-- Execute shell command and wait for its output, return Nothing on failure exit code
spawnShell' :: String -> IO (Maybe String)
spawnShell' cmd =
    runInteractiveCommand_closeFds cmd >>= \ (_, stdout, _, h) ->
        do contents <- hGetContents stdout
           -- force evaluation of contents
           exitCode <- length contents `seq` waitForProcess h
           case exitCode of
             ExitSuccess -> return $ Just contents
             _           -> return   Nothing

-- Execute shell command and wait for its output, cause exception on failure exit code
safeSpawnShell :: String -> IO String
safeSpawnShell cmd =
    spawnShell' cmd >>= f where
        f Nothing  = error $ message
        f (Just s) = return s
        message    = "shell command: " ++ cmd ++ " FAILED."

readProcessWithEnv' :: [(String,String)] -> FilePath -> [String] -> String -> IO (Maybe String)
readProcessWithEnv' extraEnv p xs i = fmap from $ readProcessWithEnvAndExitCode_closeFds extraEnv p xs i where
    from ( ExitSuccess,    o, _) = Just o
    from ((ExitFailure _), _, _) = Nothing

readProcess' :: FilePath -> [String] -> String -> IO (Maybe String)
readProcess' = readProcessWithEnv' []

readProcessOrDie :: FilePath -> [String] -> String -> IO String
readProcessOrDie p xs i = from =<< readProcess' p xs i where
    from (Just r) = return r
    from Nothing  = error $ "command: " ++ show cmd ++ " FAILED."
    cmd           = p ++ " " ++ (intercalate " " xs)

readProcessOrDieWithEnv :: [(String,String)] -> FilePath -> [String] -> String -> IO String
readProcessOrDieWithEnv extraEnv p xs i = from =<< readProcessWithEnv' extraEnv p xs i where
    from (Just r) = return r
    from Nothing  = error $ "command: " ++ show cmd ++ " FAILED."
    cmd           = p ++ (intercalate " " xs)


readProcessWithEnv_closeFds
    :: [(String,String)]        -- ^ extra environment variables
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
readProcessWithEnv_closeFds extraEnv cmd args input = do
    newEnv <- if null extraEnv then return Nothing else ((Just . (extraEnv ++)) `fmap` getEnvironment)
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit,
                                       close_fds = True,
                                       env = newEnv }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ E.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> 
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)

readProcess_closeFds
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
readProcess_closeFds = readProcessWithEnv_closeFds []

readProcessWithExitCode_closeFds = readProcessWithEnvAndExitCode_closeFds []

readProcessWithEnvAndExitCode_closeFds
    :: [(String, String)]       -- ^ extra environment variables
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithEnvAndExitCode_closeFds extraEnv cmd args input = do
    newEnv <- if null extraEnv then return Nothing else ((Just . (extraEnv ++)) `fmap` getEnvironment)
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe,
                                       close_fds = True,
                                       env = newEnv }
    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- hGetContents outh
    forkIO $ E.evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    forkIO $ E.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

runInteractiveProcess1_closeFds
  :: String
  -> CreateProcess
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess1_closeFds fun cmd = do
  (mb_in, mb_out, mb_err, p) <- 
      runGenProcess_ fun
           cmd{ std_in  = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe,
                close_fds = True}
           Nothing Nothing
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)

runInteractiveCommand_closeFds
  :: String
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveCommand_closeFds string =
  runInteractiveProcess1_closeFds "runInteractiveCommand_closeFds" (shell string)
