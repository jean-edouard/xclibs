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

{-# LANGUAGE FunctionalDependencies,MultiParamTypeClasses,FlexibleContexts,GeneralizedNewtypeDeriving,ScopedTypeVariables,FlexibleInstances,UndecidableInstances,OverlappingInstances,OverloadedStrings,PatternGuards #-}
{-# LANGUAGE TypeFamilies, GADTs #-}

module Rpc.Core
           (
             rpcGetClient
           , rpcGetSender
           -- Expose an interface to external world
           , rpcExpose
           , rpcHide
           , rpcMethod
           , rpcProperty
           -- Perform RPC calls
           , rpcCall
           , rpcCallOnce
           , rpcCallProxy
           , rpcRunParallel
           , rpcOnSignal
           , rpcOnSignalFrom
           , rpcVariantToX
           , rpcEmitSignal
           , rpcWaitForService
           , rpcRetryOnError
           , rpcDebug
           , rpcConnectTo
           , rpcConnect
           , rpcRunService
           , rpcRunServiceWithCustomStop
           , rpcServe -- deprecated
           , rpcServeOn -- deprecated
           , rpcServeWithCustomStop -- deprecated
           , rpcRequestName
           , rpcReleaseName
           , serviceNameTaken
           , liftIO
           , module Rpc.Types
           , module Rpc.Error
           , module Rpc.Monad
           , module Rpc.Domain
           , module Rpc.Variables
           , module Rpc.Remote

           , MatchRule(..), matchAll, matchAnySignal, matchSignal
                                                      
           , Dispatcher
           , BusLocation (..)
           , systemBus
           , sessionBus
           , connectBus
           ) where

import Prelude hiding (catch)
import Data.String
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.IORef
import Data.Time
import qualified Data.Text.Lazy as T

import Control.Applicative
import qualified Control.Exception as E
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans ( liftIO )
import Control.Monad.Error hiding ( liftIO )
import Control.Monad.Reader hiding ( liftIO )
import Text.Printf

import qualified Network.DBus as D
import qualified Network.DBus.Actions as D

import Rpc.Types
import Rpc.Error
import Rpc.Log
import Rpc.Domain
import Rpc.Monad
import Rpc.Variables
import Rpc.Remote
import Rpc.Domain
import Rpc.Dispatch
import Rpc.Intro

import System.Posix.Signals
import System.IO
import System.IO.Unsafe

import Tools.FreezeIOM

-- TODO: remove?
type MatchRule = D.DBusMatchRules

debugR :: IORef Bool
{-# NOINLINE debugR #-}
debugR = unsafePerformIO (newIORef False)

rpcDebug :: Bool -> IO ()
rpcDebug = writeIORef debugR

optdebug :: MonadIO m => String -> m ()
optdebug msg =
    do d <- liftIO $ readIORef debugR
       when d $ debug msg

timeddebug :: MonadIO m => String -> m a -> m a
timeddebug msg f =
    do d <- liftIO $ readIORef debugR
       if not d
          then f
          else do debug ("start: " ++ msg)
                  t0 <- liftIO getCurrentTime
                  r  <- f
                  t1 <- liftIO getCurrentTime
                  let dt = realToFrac (diffUTCTime t1 t0) :: Float
                  debug (printf "done in %.4fs: %s" dt msg)
                  return r

rpcGetClient :: MonadRpc e m => m Dispatcher
rpcGetClient = rpcGetContext >>= return . client

rpcGetSender :: MonadRpc e m => m (Maybe BusName)
rpcGetSender = rpcGetContext >>= return . messageSender

-- Single call
rpcCallOnce :: MonadRpc e m => RpcCall -> m [Variant]
rpcCallOnce theCall =
    do client  <- rpcGetClient
       reply   <- timeddebug ("call " ++ show theCall) $ liftIO $ call client theCall
       optdebug $ "reply " ++ show reply ++ " to " ++ show theCall
       case reply of
         Left error -> throwError $ fromRemoteErr theCall error
         Right v    -> return v

rpcHide :: MonadRpc e m => ObjectPath -> m ()
rpcHide path = rpcGetClient >>= \client -> liftIO $ hide client path

-- Expose a DBUS Object implementing a set of interfaces to outside world
rpcExpose :: (FreezeIOM ctx i m, MonadRpc e m) => ObjectPath -> [RpcInterface m] -> m ()
rpcExpose path interfaces = do
  client <- rpcGetClient
  freeze $ \ctx ->
    expose client path (obj ctx)
  return ()
    where
          -- the local object implementing RPC calls, which imlements a single interface
          obj c        = map (dbusI c) $ (introspectable path interfaces) : interfaces
          -- dbus interface from our interface
          dbusI c intf = let (RpcInterface intfName methods _) = intf in
                         Interface (intfName) (map (dbusM c) methods)
          -- dbus method from our method
          dbusM c m    = Method (mtNameT m) (\(sender,args) -> handleIncoming c m sender args)

rpcEmitSignal :: MonadRpc e m => RpcSignal -> m ()
rpcEmitSignal signal = do
    client  <- rpcGetClient
    liftIO $ emit client signal

rpcOnSignal :: 
   ( FreezeIOM ctx i m, MonadRpc e m ) 
  => MatchRule -> (BusName -> RpcSignal -> m ()) -> m ()
rpcOnSignal rule f = do
    client  <- rpcGetClient
    freeze $ \context ->
       hookSignal client rule $ \sender sig -> do
         handle =<< processInIO' context (f sender sig)
  where         
    handle (Left ex) = warn ("error during signal processing: " ++ show ex)
    handle _ = return ()
          
rpcOnSignalFrom :: 
   ( FreezeIOM ctx i m, MonadRpc e m ) 
  => Proxy -> String -> (RpcSignal -> m ()) -> m ()
rpcOnSignalFrom proxy name f = do
    client  <- rpcGetClient
    let Proxy (RemoteObject busname path) iname = proxy
    freeze $ \context ->
      hookSignalFrom client busname path iname (fromString name) $ \sig ->
        handle =<< processInIO' context (f sig)
  where         
    handle (Left ex) = warn ("error during signal processing: " ++ show ex)
    handle _ = return ()
          
rpcMethod :: MonadRpc e m => String -> String -> String -> ( [Variant] -> m [Variant] ) -> RpcMethod m
rpcMethod name ins outs body = RpcMethod {
                              mtSigIn  = rpcParseSigStr ins
                            , mtSigOut = rpcParseSigStr outs
                            , mtNameT  = mkMemberName_ $ T.pack name
                            , mtInvoke = body
                            }

rpcProperty :: String -> String -> PropertyAccess -> RpcProperty
rpcProperty name typeSig access = RpcProperty {
                                    propNameT= mkMemberName_ $ T.pack name
                                  , propType = typ
                                  , propAccess = access }
    where
      RpcParam _ typ = head $ rpcParseSigStr typeSig

-- Create an interface which introspects another intrerface
introspectable :: MonadRpc e m => ObjectPath -> [RpcInterface m] -> RpcInterface m
introspectable path interfaces =
    RpcInterface "org.freedesktop.DBus.Introspectable"
                     [
                       rpcMethod "Introspect" "" "s" (\args -> return [toVariant $ introXml path interfaces] )
                     ]
                     [ ] -- no properties

-- Handle incoming RPC requests. We fork a lightweight IO thread for processing each request
-- This doesn't consume much resources
handleIncoming ::
   ( FreezeIOM ctx i m, MonadRpc e m ) 
  => ctx -> RpcMethod m -> Maybe BusName -> [Variant] -> IO (Either RemoteErr [Variant])
handleIncoming context method sender args = do
         -- We execute RPC monad, which either succeeds or finishes with an error.
         -- When we have a failure, we convert it to DBUS error reply with call information
         -- and recorded error
         -- also we trap all the IO exceptions and send dbus error replies
         timeddebug ("process method " ++ show method) $
           handle =<< processInIO' context (withSender $ mtInvoke method args)
  where
    handle (Right v)  = return (Right v)
    handle (Left err) = do
      warn ("exception: " ++ show err ++ " while processing RPC " ++ show method)
      return . Left $ RemoteErr "org.freedesktop.DBus.Error.Failed" [toVariant err]               
    withSender = rpcLocalContext $ \c -> c { messageSender = sender }

data ProcessRes e a =
    IOError E.SomeException
  |   Error e
  |   Value a


processInIO' :: ( FreezeIOM ctx i m, MonadRpc e m ) => ctx -> m a -> IO (Either String a)
processInIO' ctx f = return . from =<< processInIO ctx f where
  from (IOError err) = Left (show err)
  from (  Error err) = Left (show err)
  from (  Value v  ) = Right v

processInIO :: ( FreezeIOM ctx i m, MonadRpc e m ) => ctx -> m a -> IO (ProcessRes e a)
processInIO ctx f = do
  r <- newEmptyMVar
  rIO <-
    E.try . thaw ctx $
        ( f                >>= liftIO . putMVar r . Right )
          `catchError`       ( liftIO . putMVar r . Left  )
  case rIO of
    Left err   -> return (IOError err)
    Right _    -> takeMVar r >>= \r' -> case r' of
      Left err -> return (Error err)
      Right v  -> return (Value v)
  
-- Wait until service name appears on DBUS
rpcWaitForService :: MonadRpc e m => String -> m ()
rpcWaitForService name = do
    t <- serviceNameTaken name
    case t of
      True  -> return ()
      -- Keep waiting..
      False -> liftIO (threadDelay (10^6)) >> rpcWaitForService name

-- Perform an action, retrying all of it if retryCheck function says so
rpcRetryOnError :: MonadRpc e m => Int -> Int -> (e -> Bool) -> m a -> m a
rpcRetryOnError maxTries delayBetweenTriesMs retryCheck action
    | maxTries <= 0   = error "maxTries cannot be <= 0"
    -- If just one try, we do not catch errors here
    | maxTries == 1   = action
    -- Otherwise we do
    | otherwise       = action `catchError` onError
  where
    -- retry only if passed function says so
    onError err | not . retryCheck $ err = throwError err
    onError err | otherwise =
        do liftIO $ do
             warn $ "Retrying action because of error: " ++ show err
             threadDelay (1000 * delayBetweenTriesMs)
           rpcRetryOnError (maxTries-1) delayBetweenTriesMs retryCheck action

-- Check if DBUS service name is taken already
serviceNameTaken :: MonadRpc e m => String -> m Bool
serviceNameTaken name = do
  args <- rpcCall (RpcCall "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner" [toVariant name])
  -- this should return a single boolean
  case map fromVariant args of
    [Just flag] -> return flag
    _           -> error "impossible, unexpected reply"

serviceUnknownDbusErrorName = "org.freedesktop.DBus.Error.ServiceUnknown"

-- Actually perform an RPC call, in a blocking fashion
rpcCall :: MonadRpc e m => RpcCall -> m [Variant]
rpcCall call =
    -- we retry RPC calls if service is temporarily unavailable, for 5s
    rpcRetryOnError 5 1000 retryCheck $ rpcCallOnce call
    where
      retryCheck e = case toRemoteErr e of
                       Nothing -> False
                       Just (RemoteErr name _) -> name == serviceUnknownDbusErrorName

rpcCallProxy :: MonadRpc e m => Proxy -> String -> [Variant] -> m [Variant]
rpcCallProxy (Proxy (RemoteObject dest path) intf) mem args
  = rpcCall $ RpcCall dest path intf (fromString mem) args

sig_ :: String -> D.Signature
sig_ [] = []
sig_ x =  either error id $ D.unserializeSignature (fromString x)

-- Parse parameter definition string and get signature + type names
rpcParseSigStr :: String -> [RpcParam]
rpcParseSigStr str
    -- no names given
    | not (':' `elem` str) =
        let mkParam (t,n) = RpcParam n t in
        map mkParam $ zip (sig_ str) (cycle [""])
    -- names because we have a colon there
    | otherwise =
        let paramStrs = map strip $ split ',' str
            pairs     = map (split ':') paramStrs
         in map parametrise pairs
        where
          -- param without name
          parametrise (p:[])   = RpcParam "" (head . sig_ $ p)
          -- param with name
          parametrise (n:p:[]) = RpcParam (T.pack n)  (head . sig_ $ p)
          -- bad parameter
          parametrise _        = error "bad parameter string!"


rpcGetSig :: [RpcParam] -> D.Signature
rpcGetSig params =
  map getType params where getType (RpcParam _ t) = t

rpcVariantToX :: (Variable a) => Variant -> a
rpcVariantToX v = case fromVariant v of
                      Nothing -> error $ "failed to coerce variant " ++ show v ++ " to expected type"
                      Just x  -> x

-- execute rpc actions in parallel, aggregate results
rpcRunParallel :: (FreezeIOM ctx i m, MonadRpc e m) => [m a] -> m [a]
rpcRunParallel acts = do
   mvars <- freeze $ \ctx -> mapM (fork ctx) acts
   mapM take mvars
    where
      take mv = (liftIO . takeMVar) mv >>= cont where
      fork ctx action = do
        mv <- newEmptyMVar
        forkIO (putMVar mv =<< thaw ctx action)
        return mv

-- Split a list over an element
split :: (Eq a) => a -> [a] -> [[a]]
split sep xs =
    let (y,ys) = span (/= sep) xs in
    case ys of
      [] -> [y]
      zs -> y : split sep (tail zs)


-- Strip a string from whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

matchAll = D.defaultDBusMatchRules
matchAnySignal = matchAll { D.matchType = Just D.TypeSignal }
matchSignal intf memb = matchAnySignal { D.matchInterface = Just intf
                                       , D.matchMember = Just memb }

data BusLocation
   = SystemBus | SessionBus | DomainBus Int

rpcConnectTo :: BusLocation -> IO RpcContext
rpcConnectTo = go where
  go SystemBus          = systemBus >>= connectBus >>= makeContext
  go SessionBus         = sessionBus >>= connectBus >>= makeContext
  go (DomainBus domid)  = rpcTryConnectDomainBus domid 120 >>= makeContext2 domid
  makeContext (disp,_)  = rpcMkContext disp
  makeContext2 domid Nothing  = error $ "failed to connect to domain's " ++ show domid ++ " dbus"
  makeContext2 domid (Just d) = rpcMkContext d
  
rpcConnect :: IO RpcContext
rpcConnect = rpcConnectTo SystemBus

rpcRunService name f = rpcRunServiceWithCustomStop name (return ()) f
rpcRunServiceWithCustomStop :: forall ctx i e m a . (FreezeIOM ctx i m, MonadRpc e m) => String -> m () -> m a -> m a
rpcRunServiceWithCustomStop name customStop f =
  cont =<< ( freeze $ \ctx ->
    do start ctx
       E.finally (act ctx) (stop ctx)
    )
  where
      act ctx =
        do installHandler sigTERM (onSig sigTERM ctx) Nothing
           installHandler sigINT  (onSig sigINT  ctx) Nothing
           thaw ctx f
      start, stop :: ctx -> IO ()
      start ctx = thaw ctx (rpcRequestName name :: m ()) >> return ()
      stop  ctx = thaw ctx ((customStop >> rpcReleaseNames) :: m ()) >> return ()
      
      onSig sig ctx = CatchOnce $ do
        stop ctx
        raiseSignal sig

rpcRequestName :: MonadRpc e m => String -> m ()
rpcRequestName name =
  do c <- rpcGetContext
     let name' = fromString name
         cli   = client c
         entry = (name, cli)
     liftIO $ do
       requestName cli name'
       modifyMVar_ (names c) $ return . (entry :)
     
rpcReleaseName :: MonadRpc e m => String -> m ()
rpcReleaseName name =
  do c <- rpcGetContext
     liftIO $ modifyMVar_ (names c) release
  where
    release [] = return []
    release (x@(n,cli):xs)
      | n == name = releaseName cli (fromString n) >> release xs
      | otherwise = (:) <$> pure x <*> release xs

rpcReleaseNames :: MonadRpc e m => m ()
rpcReleaseNames =
  do c <- rpcGetContext
     liftIO $ modifyMVar_ (names c) release
  where
    release [] = return []
    release (x@(n,cli):xs) = releaseName cli (fromString n) >> release xs
     
{- deprecated -}

rpcServeOn :: Bool -> String -> (RpcContext -> IO a) -> IO a
rpcServeOn session name f = rpcServeWithCustomStopOn session name (\_ -> return ()) f

rpcServe :: String -> (RpcContext -> IO a) -> IO a
rpcServe = rpcServeOn False

rpcServeWithCustomStopOn :: Bool
  -> String
  -> (RpcContext -> IO ())
  -> (RpcContext -> IO a)
  -> IO a
rpcServeWithCustomStopOn session name custom_stop f =
  E.bracket (serviceStart session name) stop act where
    act ctx =
      do installHandler sigTERM (onSig sigTERM ctx) Nothing
         installHandler sigINT  (onSig sigINT  ctx) Nothing
         f ctx
    onSig sig ctx =
      CatchOnce $ do
        stop ctx
        raiseSignal sig
    stop ctx
      = custom_stop ctx >> serviceStop ctx

rpcServeWithCustomStop :: String
  -> (RpcContext -> IO ())
  -> (RpcContext -> IO a)
  -> IO a
rpcServeWithCustomStop = rpcServeWithCustomStopOn False

serviceStart :: Bool -> String -> IO RpcContext
serviceStart session serviceName =
    do info "starting RPC server"
       (disp, _) <- connectBus =<< (if session then sessionBus else systemBus)
       requestName disp (fromString serviceName)
       rpcMkContext disp
       
serviceStop :: RpcContext -> IO ()
serviceStop context =
    do info "stopping RPC server"
       modifyMVar_ (names context) $ \names -> 
         do mapM_ (releaseName (client context) . fromString . fst) names
            return []
       return ()
