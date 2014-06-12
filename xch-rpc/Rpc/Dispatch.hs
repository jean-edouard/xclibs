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

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Rpc.Dispatch
       ( Dispatcher
       , Interface(..), Method(..)
       , systemBus
       , sessionBus
       , connectBus
       , dispatch
       , expose
       , hide
       , call
       , emit
       , hookSignal
       , hookSignalFrom
       , requestName
       , releaseName
       ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Maybe
import Data.String
import Data.Word
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL

import qualified Network.DBus as D
import qualified Network.DBus.Actions as D

import Rpc.Types
import Rpc.Error
import Rpc.Log
import Rpc.Variables

data Dispatcher
   = Dispatcher { conn      :: !D.DBusContext
                , callCh    :: Chan Received
                , signalCh  :: Chan Received
                , objects   :: MVar (Map ObjectPath [Interface])
                , callbacks :: MVar (Map Serial Callback)
                , signalCbs :: MVar [ Callback ]
                , sendLock  :: MVar ()
                }

type Callback = Received -> IO ()
type Serial = D.Serial

data Interface = Interface !InterfaceName ![Method]
data Method = Method !MemberName ((Maybe BusName, [Variant]) -> IO (Either RemoteErr [Variant]))

data Received
   = ReceivedCall !Serial (Maybe BusName) !D.DBusCall
   | ReceivedSignal !Serial (Maybe BusName) !D.DBusSignal
   | ReceivedReturn !Serial (Maybe BusName) !D.DBusReturn
   | ReceivedError !Serial (Maybe BusName) !D.DBusError
   | ReceivedUnknown


systemBus :: IO D.DBusContext
systemBus = D.busGetSystem

sessionBus :: IO D.DBusContext
sessionBus = D.busGetSession

connectBus :: D.DBusContext -> IO (Dispatcher, BusName)
connectBus c =
  do d <- newDispatcher c
     D.authenticateWithRealUID c
     forkIO $ dispatch d
     name <- hello d
     return (d, name)

newDispatcher :: D.DBusContext -> IO Dispatcher
newDispatcher c = Dispatcher <$> pure c <*> newChan <*> newChan <*> newMVar Map.empty <*> newMVar Map.empty <*> newMVar [] <*> newMVar ()

withSendLock :: Dispatcher -> IO a -> IO a
withSendLock d f = withMVar (sendLock d) $ \() -> f

send :: Dispatcher -> (Serial -> IO ()) -> D.DBusMessage -> IO ()
send d withSerial msg
  = withSendLock d $ do
      serial <- D.busGetNextSerial (conn d)
      withSerial serial
      D.messageSendWithSerial (conn d) serial msg

sendTo :: Dispatcher -> (Serial -> IO ()) -> BusName -> D.DBusMessage -> IO ()
sendTo d withSerial dest m =
  send d withSerial (setDestination dest m)

send' :: Dispatcher -> Maybe BusName -> D.DBusMessage -> IO ()
send' d Nothing m = send d (const $ return()) m
send' d (Just dest) m = sendTo d (const $ return()) dest m

sendWithReply :: Dispatcher -> D.DBusMessage -> IO Received
sendWithReply d msg
  = do r <- newEmptyMVar
       send d (installCb r) msg
       takeMVar r
    where
      installCb r serial = modifyMVar_ (callbacks d) $ pure . Map.insert serial (callback serial r)
      callback serial r msg
        = do modifyMVar_ (callbacks d) $ pure . Map.delete serial
             putMVar r msg

sendErrorReply :: Dispatcher -> Serial -> (Maybe BusName) -> ErrorName -> [D.DBusValue] -> IO ()
sendErrorReply di s to err args =
  send' di to . D.toDBusMessage $ D.DBusError s (TL.unpack $ strErrorName err) args

sendReturnReply :: Dispatcher -> Serial -> (Maybe BusName) -> [Variant] -> IO ()
sendReturnReply di s to args =
  send' di to . D.toDBusMessage $ D.DBusReturn s args

call :: Dispatcher -> RpcCall -> IO (Either RemoteErr [DBusValue])
call d c
  = fromReply <$> sendWithReply d (callToDbusMsg c)
    where
      fromReply (ReceivedReturn serial from m) = Right $ D.returnBody m
      fromReply (ReceivedError serial from m) = Left . mkerror $ m
      fromReply _ = internalErr "invalid reply message"
      
      internalErr (s :: String) = Left $ RemoteErr internalFailed [toVariant s]
      mkerror e = RemoteErr (fromString $ D.errorName e) (D.errorBody e)

call_ :: Dispatcher
  -> RpcCall
  -> IO [Variant]
call_ di c = handle =<< call di c where
  handle (Left err) = E.throw err
  handle (Right xs) = return xs
  
expose :: Dispatcher -> ObjectPath -> [Interface] -> IO ()
expose di path intfs
  = modifyMVar_ (objects di) $ pure . Map.insert path intfs

hide :: Dispatcher -> ObjectPath -> IO ()
hide di path
  = modifyMVar_ (objects di) $ pure . Map.delete path
    
emit :: Dispatcher
  -> RpcSignal
  -> IO ()
emit d = send d (const $ return()) . signalToDbusMsg

-- TODO: destination field match somehow?
matchesSignal :: D.DBusMatchRules -> Received -> Bool
matchesSignal r (ReceivedSignal _ sender sig)
  = all ( ==True ) 
    [ m (D.matchSender r) ((TL.unpack.strBusName) `fmap` sender)
    , m (D.matchInterface r) (Just $ D.signalInterface sig)
    , m (D.matchMember r) (Just $ D.signalMember sig)
    , m (D.matchPath r) (Just $ D.signalPath sig)
    ]
    where
      m :: Eq a => Maybe a -> Maybe a -> Bool
      m Nothing  _        = True
      m (Just x) Nothing  = False
      m (Just x) (Just y) = x == y
      
matchesSignal _ _ = False

hookSignal :: Dispatcher
  -> D.DBusMatchRules
  -> (BusName -> RpcSignal -> IO ())
  -> IO ()
hookSignal di mr' handler =
 do register
    modifyMVar_ (signalCbs di) $ pure . (cb:)
    where
      register = addMatch di mr where
      mr = mr' { D.matchType = Just D.TypeSignal }
      cb m@(ReceivedSignal _ (Just sender) s)
        | mr `matchesSignal` m = errors . handler sender $
                           RpcSignal { signalPath = fromString . show $ D.signalPath s
                                     , signalMemberT = fromString $ D.signalMember s
                                     , signalInterfaceT = fromString $ D.signalInterface s
                                     , signalArgs = D.signalBody s }
      cb _ = return ()
      
      errors f = f `E.catch` report where
        report (er :: E.SomeException) = warn $ "Error during signal processing: " ++ show er

hookSignalFrom :: Dispatcher
  -> BusName -> ObjectPath -> InterfaceName -> MemberName
  -> (RpcSignal -> IO ())
  -> IO ()
hookSignalFrom di dst path iface memb handler
  = hookSignal di mr (\_ s -> handler s) where
    mr = D.DBusMatchRules { D.matchType = Nothing
                          , D.matchSender = Just (TL.unpack $ strBusName $ dst)
                          , D.matchInterface = Just (TL.unpack $ strInterfaceName $ iface)
                          , D.matchMember = Just (TL.unpack $ strMemberName $ memb)
                          , D.matchPath = Just (fromString $ TL.unpack $ strObjectPath $ path)
                          , D.matchDestination = Nothing
                          }

findCallback :: Dispatcher -> Serial -> IO (Maybe Callback)
findCallback di s =
  do cs <- readMVar (callbacks di)
     return $ Map.lookup s cs

findMethod :: (Maybe InterfaceName) -> MemberName -> [Interface] -> Maybe Method
findMethod Nothing  _ _  = Nothing
findMethod (Just iname) m is =
  method m =<< imethods
  where
    method name [] = Nothing
    method name (m@(Method n _):ms)
      | name == n = Just m
      | otherwise = method name ms
    imethods = look is where
      look [] = Nothing
      look ((Interface name ms) : is)
        | name == iname = Just ms
        | otherwise     = look is

callname c =
  maybe TL.empty (\i -> TL.pack i `TL.append` ".") (D.callInterface c)
  `TL.append`
  TL.pack (D.callMember c)

mkReceived :: D.DBusMessage -> Received
mkReceived m
  = case D.msgType m of
      D.TypeMethodCall -> wrap ReceivedCall m
      D.TypeSignal -> wrap ReceivedSignal m
      D.TypeMethodReturn -> wrap ReceivedReturn m
      D.TypeError -> wrap ReceivedError m
      _ -> ReceivedUnknown
    where
      wrap cons m = fromMaybe ReceivedUnknown $ cons (D.msgSerial m) (sender m) <$> D.fromDBusMessage m
      
dispatch :: Dispatcher -> IO ()
dispatch di = do
  forkIO $ readCalls (callCh di)
  forkIO $ readSignals (signalCh di)
  loop
  where
    loop = (process . mkReceived =<< D.messageRecv (conn di)) >> loop
    process m@( ReceivedSignal{} ) = writeChan (signalCh di) m
    process m@( ReceivedCall{}   ) = writeChan (callCh di) m
    process m = forkIO (handle_ m) >> return ()

    readCalls   ch = (forkIO . handle_ =<< readChan ch) >> readCalls ch
    readSignals ch = (         handle_ =<< readChan ch) >> readSignals ch

    handle_ m = check =<< E.try (handle di m) where
      check (Left (ex :: E.SomeException)) = warn $ "io error: " ++ show ex
      check (Right v) = return ()

handle :: Dispatcher -> Received -> IO ()
handle di (ReceivedCall serial from call) = do
  objs <- readMVar (objects di)
  let p = fromString . show $ D.callPath call
      intf = fromString `fmap` D.callInterface call
      memb = fromString $ D.callMember call
  case Map.lookup p objs of
    Nothing -> sendErrorReply di serial from unknownObject [toVariant $ p]
    Just o  -> case findMethod intf memb o of
      Nothing -> sendErrorReply di serial from unknownMethod [toVariant $ memb]
      Just (Method _ invoke)  -> do
        r <- invoke (from, D.callBody call)
        case r of
          Left x@(RemoteErr errname body) ->
            do warn $ show x ++ " while processing RPC " ++ TL.unpack (callname call) 
               sendErrorReply di serial from errname body
          Right xs ->
            sendReturnReply di serial from xs

handle di m@(ReceivedSignal serial from sig) = do
  cbs <- readMVar (signalCbs di)
  mapM_ (\cb -> cb m) cbs
  
handle di m@(ReceivedError serial from err) = do
  cb <- findCallback di (D.errorReplySerial err)
  case cb of
    Nothing -> return ()
    Just cb -> cb m

handle di m@(ReceivedReturn serial from ret) = do
  cb <- findCallback di (D.returnReplySerial ret)
  case cb of
    Nothing -> return ()
    Just cb -> cb m

handle _ _ = warn "invalid message received"

destination :: D.DBusMessage -> Maybe BusName
destination m = mkBusName_ . TL.pack <$> D.fieldsDestination (D.msgFields m)

sender :: D.DBusMessage -> Maybe BusName
sender m = mkBusName_ . TL.pack <$> D.fieldsSender (D.msgFields m)

setDestination :: BusName -> D.DBusMessage -> D.DBusMessage
setDestination n m =
  m { D.msgFields = (D.msgFields m) { D.fieldsDestination = Just n' } }
  where n' = TL.unpack $ strBusName n

-- conversions
callToDbusMsg :: RpcCall -> D.DBusMessage
callToDbusMsg c =
  setDestination (callDest c) $ D.toDBusMessage c'
  where
    dest' = TL.unpack $ strBusName $ callDest c
    c' =
      D.DBusCall { D.callPath = fromString (TL.unpack $ strObjectPath $ callPath c)
                 , D.callMember = TL.unpack $ strMemberName $ callMemberT c
                 , D.callInterface = Just $ TL.unpack $ strInterfaceName $ callInterfaceT c
                 , D.callBody = callArgs c
                 }

signalToDbusMsg :: RpcSignal -> D.DBusMessage
signalToDbusMsg s =
  D.toDBusMessage s'
  where
    s' =
      D.DBusSignal { D.signalPath = fromString . TL.unpack . strObjectPath $ signalPath s
                   , D.signalMember = TL.unpack . strMemberName $ signalMemberT s
                   , D.signalInterface = TL.unpack . strInterfaceName $ signalInterfaceT s
                   , D.signalBody = signalArgs s }

unknownObject, unknownMethod, failed, internalFailed :: ErrorName
unknownObject = fromString "org.freedesktop.DBus.Error.UnknownObject"
unknownMethod = fromString "org.freedesktop.DBus.Error.UnknownMethod"
failed = fromString "org.freedesktop.DBus.Error.Failed"
internalFailed = fromString "org.freedesktop.DBus.Error.Failed.Internal" -- shouldn't happen

hello :: Dispatcher -> IO BusName
hello d = do
  v <- call_ d msgHello
  return $ case v of
    [x] -> fromString $ fromMaybe (error "hello: error unpacking variant") (fromVariant x)
    _   -> error "hello: bad reply from bus"
  
requestName :: Dispatcher -> BusName -> IO ()
requestName di b = do
  v <- call_ di (msgRequestName b)
  case v of
    [x] -> do let (rv :: Word32) = fromMaybe (error "requestName: error unpacking variant") (fromVariant x)
              when (rv /= 1) $ error "requestName: failed"    
    _ -> error "requestName: bad reply from bus"

releaseName :: Dispatcher -> BusName -> IO ()
releaseName di b = call_ di (msgReleaseName b) >> return()
  
addMatch :: Dispatcher -> D.DBusMatchRules -> IO ()
addMatch d mr = call_ d (msgAddMatch mr) >> return()

msgRequestName :: BusName -> RpcCall
msgRequestName b
  = RpcCall { callDest = fromString "org.freedesktop.DBus"
            , callPath = fromString "/org/freedesktop/DBus"
            , callInterfaceT = fromString "org.freedesktop.DBus"
            , callMemberT = fromString "RequestName"
            , callArgs = [ toVariant b, toVariant (0 :: Word32) ] }

msgReleaseName :: BusName -> RpcCall
msgReleaseName b
  = RpcCall { callDest = fromString "org.freedesktop.DBus"
            , callPath = fromString "/org/freedesktop/DBus"
            , callInterfaceT = fromString "org.freedesktop.DBus"
            , callMemberT = fromString "ReleaseName"
            , callArgs = [ toVariant b ] }

msgAddMatch :: D.DBusMatchRules -> RpcCall
msgAddMatch mr
  = RpcCall { callDest = fromString "org.freedesktop.DBus"
            , callPath = fromString "/org/freedesktop/DBus"
            , callInterfaceT = fromString "org.freedesktop.DBus"
            , callMemberT = fromString "AddMatch"
            , callArgs = [ toVariant serialized ] }
    where    
      serialized = intercalate "," $ filter (not . null)
                   [ mm "type"        show $ D.matchType mr
                   , mm "sender"      id $ D.matchSender mr
                   , mm "interface"   id $ D.matchInterface mr
                   , mm "member"      id $ D.matchMember mr
                   , mm "path"        D.unObjectPath $ D.matchPath mr
                   , mm "destination" id $ D.matchDestination mr
                   ]
      mm key f = maybe "" (surroundQuote key . f)
      surroundQuote key v = concat [ key, "='",  v, "'" ]

msgHello :: RpcCall
msgHello
  = RpcCall { callDest = fromString "org.freedesktop.DBus"
            , callPath = fromString "/org/freedesktop/DBus"
            , callInterfaceT = fromString "org.freedesktop.DBus"
            , callMemberT = fromString "Hello"
            , callArgs = [ ] }
