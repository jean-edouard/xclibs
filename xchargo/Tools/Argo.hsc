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

{-# LANGUAGE CPP,ForeignFunctionInterface #-}
module Tools.Argo ( Addr (..)
                 , DomID
                 , SocketType (..)
                 , socket, close, bind, connect, listen, accept, send, recv
                 ) where

import Data.Word
import qualified Data.ByteString as B
import Data.ByteString.Internal ( createAndTrim )
import Data.ByteString.Unsafe( unsafeUseAsCStringLen )
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception
import Foreign
import Foreign.C.Types
import Foreign.C.Error
import System.Posix.Types
import Network.Socket ( SocketType, packSocketType )
import System.IO
import System.IO.Error
import System.Posix.IO
import Tools.Log
import Data.Bits
import System.Environment

-- bit of networking boilerplate
#include <sys/socket.h>
newtype SocketLevel = SocketLevel { socket_level :: CInt } deriving Eq

#{enum SocketLevel, SocketLevel
  , sol_socket = SOL_SOCKET }

newtype SocketOption = SocketOption { socket_option :: CInt } deriving Eq

#{enum SocketOption, SocketOption
 , so_error = SO_ERROR }

type DomID = Int

data Addr = Addr { addrPort  :: !Int
                 , addrDomID :: !DomID } deriving Show

#include <libargo.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Addr where
    alignment _ = #{alignment xen_argo_addr_t}
    sizeOf    _ = #{size xen_argo_addr_t}
    peek p      = do port  <- #{peek xen_argo_addr_t, aport} p
                     domid <- ((.&.) 0xFFFF) <$> #{peek xen_argo_addr_t, domain_id} p
                     return $ Addr port domid
    poke p v    = do #{poke xen_argo_addr_t, aport} p (addrPort v)
                     #{poke xen_argo_addr_t, domain_id} p (addrDomID v)

-- subset of libargo.h
foreign import ccall "libargo.h argo_socket" c_argo_socket    :: CInt -> IO CInt
foreign import ccall "libargo.h argo_close" c_argo_close      :: CInt -> IO CInt
foreign import ccall "libargo.h argo_bind" c_argo_bind        :: CInt -> Ptr Addr -> CInt -> IO CInt
foreign import ccall "libargo.h argo_connect" c_argo_connect  :: CInt -> Ptr Addr -> IO CInt
foreign import ccall "libargo.h argo_listen" c_argo_listen    :: CInt -> CInt -> IO CInt
foreign import ccall "libargo.h argo_accept" c_argo_accept    :: CInt -> Ptr Addr -> IO CInt
foreign import ccall "libargo.h argo_send" c_argo_send        :: CInt -> Ptr Word8 -> CULong -> CInt -> IO CLong 
foreign import ccall "libargo.h argo_recv" c_argo_recv        :: CInt -> Ptr Word8 -> CULong -> CInt -> IO CLong
foreign import ccall "libargo.h argo_getsockopt" c_argo_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> Ptr Int -> IO Int

int :: (Integral a, Num b) => a -> b
int = fromIntegral

socket :: SocketType -> IO Fd
socket t =
    do fd <- int <$> throwErrnoIfMinus1 "socket" ( c_argo_socket (packSocketType t) )
       setFdOption fd NonBlockingRead True
       return fd

close :: Fd -> IO ()
close f = throwErrnoIfMinus1 "close" ( c_argo_close (int f) ) >> return ()

bind :: Fd -> Addr -> DomID -> IO ()
bind f addr partner = do
    with addr $ \addr_p ->
        throwErrnoIfMinus1 "bind" $ c_argo_bind (int f) addr_p (int partner)
    return ()

maybeBindClient :: Fd -> Addr -> IO ()
maybeBindClient f addr = do
    do
       envAddend <- getEnv "ARGO_CLIENT_PORT_ADDEND"
       let addend = read envAddend::Int
       bind f (Addr (addrPort addr + addend) 0x7FF4) (addrDomID addr)
       return ()
    `Control.Exception.catch` \e -> do
       if (System.IO.Error.isDoesNotExistError e)
          then return ()
          else throw e

connect :: Fd -> Addr -> IO ()
connect f addr = do
    with addr $ \addr_p ->
        let connect_loop =
                do maybeBindClient f addr
                   r <- c_argo_connect (int f) addr_p
                   if r == -1
                     then do err <- getErrno
                             case () of
                               _ | err == eINTR       -> connect_loop
                               _ | err == eINPROGRESS -> connect_blocked
                               _ | err == eAGAIN      -> connect_blocked
                               otherwise              -> throwErrno "connect"
                     else return ()
            connect_blocked =
                do threadWaitWrite f
                   err <- getsockopt f so_error
                   when (err /= 0) $
                        ioError ( errnoToIOError "connect" (Errno (fromIntegral err)) Nothing Nothing )
        in connect_loop

listen :: Fd -> Int -> IO ()
listen f backlog = do
    throwErrnoIfMinus1 "listen" $ c_argo_listen (int f) (int backlog)
    return ()

accept :: Fd -> IO (Fd, Addr)
accept f =
    alloca $ \addr_p ->
        do f' <- throwErrnoIfMinus1RetryMayBlock "accept" (c_argo_accept (int f) addr_p) (threadWaitRead f)
           setFdOption (int f') NonBlockingRead True
           addr <- peek addr_p
           return (int f', addr)

send :: Fd -> B.ByteString -> Int -> IO Int
send f buf flags =
    fmap int $
         unsafeUseAsCStringLen buf $ \(ptr,sz) ->
             throwErrnoIfMinus1RetryMayBlock "send"
             ( c_argo_send (int f) (castPtr ptr) (int sz) (int flags) )
             ( moan f buf flags >> threadDelay (5 * 10^5) >> threadWaitWrite f )

moan :: Fd -> B.ByteString -> Int -> IO ()
moan fd buf flags = do
    warn $ "ALERT! EAGAIN trying to send over argo fd=" ++ show fd
             ++ " flags=" ++ show flags
             ++ ", data_len=" ++ show (B.length buf)
             ++ " data follows:"
    warn $ show buf

recv :: Fd -> Int -> Int -> IO B.ByteString
recv f sz flags =
    createAndTrim sz $ \ptr ->
        fmap int $
             throwErrnoIfMinus1RetryMayBlock "recv"
             ( c_argo_recv (int f) (castPtr ptr) (int sz) (int flags) )
             ( threadWaitRead f )

getsockopt :: Fd -> SocketOption -> IO Int
getsockopt fd option | option == so_error =
    fmap fromIntegral $
    alloca $ \buffer ->
    alloca $ \len_buffer ->
        do poke len_buffer (sizeOf $ (undefined :: CInt))
           throwErrnoIfMinus1 "getsockopt" ( c_argo_getsockopt
                                             (int fd) 
                                             (socket_level sol_socket)
                                             (socket_option so_error)
                                             (castPtr buffer)
                                             len_buffer )
           peek ( buffer :: Ptr CInt )
getsockopt fd _ = error "unsupported option type"
