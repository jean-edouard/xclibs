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

{-# LANGUAGE  PatternGuards, DeriveDataTypeable, BangPatterns #-}

module Network.WebSocket
       ( WebSocketErr(..)
       , WebSocket
       , AppDataType(..)
       , AppDataFrame
       , create
       , shutdown
       , handshake
       , recvFrame
       , sendFrame
       )

       where
import Data.Char
import Data.Bits
import Data.List
import Data.Word
import Data.Int
import Data.Typeable
import Data.Binary
import Control.Monad
import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import System.Process
import System.Exit
import System.IO.Unsafe

data WebSocket
   = WebSocket {
       recv   :: Int -> IO B.ByteString
     , send   :: B.ByteString -> IO Int
     , buffer :: MVar BL.ByteString
     , closed :: MVar Bool
     }
     
-- | websocket stream errors
data WebSocketErr
   = ExpectedLine
   | ExpectedHttpHdr
   | ExpectedByte
   | InvalidGetRequest
   | InvalidOpcode
   | InvalidControlFrame
   | InvalidPayloadLen
   | InvalidFrame
   | LineTooLong
   | PayloadTooShort
   | PayloadTooLong Int64
   | TooManyHttpHeaders
     deriving (Eq,Show,Typeable)

instance E.Exception WebSocketErr

-- | create socket using receive/send function pair
create :: (Int -> IO B.ByteString) -> (B.ByteString -> IO Int) -> IO WebSocket
create r s
  = WebSocket r s <$> (newMVar =<< incoming r)
                  <*> (newMVar False)
-- | handshake with client
handshake :: WebSocket -> IO ()
handshake s = respond =<< request where
  request = consume_ s reqP
  respond = sendAll' s . httpResponse . response

-- | shutdown using Close handshake
shutdown :: WebSocket -> IO ()
shutdown s =
  modifyMVar_ (closed s) $ \c ->
    do when (not c) $ sendClose s >> recvClose s
       return True
       
data Frame
   = Frame { frmFIN  :: Bool
           , frmOp :: Opcode
           , frmData :: BL.ByteString
           } 
   deriving (Eq, Show)

data Opcode
   = Cont | App AppDataType | Close | Ping | Pong
   deriving (Eq, Show)

data AppDataType
   = Text | Binary
   deriving (Eq, Show)

type AppDataFrame = (AppDataType, BL.ByteString)

isControlOp Close = True
isControlOp Ping  = True
isControlOp Pong  = True
isControlOp _     = False

-- | receive a frame. Return Nothing on client initiated close
recvFrame :: WebSocket -> Int -> IO (Maybe AppDataFrame)
recvFrame s maxLen = rec Nothing 0 where
  rec :: Maybe AppDataType -> Int64 -> IO (Maybe AppDataFrame)
  rec dataT runlen = frame =<< consume s (frameP $ maxLen - fromIntegral runlen) where
    frame (Left er) = return Nothing
    frame (Right f) = case (frmOp f, frmFIN f) of
      (App t, True ) -> return $ Just (t, frmData f)
      (App t, False) -> fragmented t
      (Cont , False) -> fragmentedCont
      (Cont , True ) -> E.throw InvalidFrame
      (Ping , _    ) -> sendPong s f >> rec dataT runlen
      (Pong , _    ) -> rec dataT runlen
      (Close, _    ) -> modifyMVar_ (closed s) (\_ -> sendClose s >> return True) >> return Nothing
      where
        runlen' = runlen + BL.length (frmData f)
        fragmented t
          | runlen' > fromIntegral maxLen = E.throw (PayloadTooLong runlen')
          | otherwise                     = more =<< rec (Just t) runlen'
            where more Nothing       = return $ Just (t, frmData f)
                  more (Just (_, x)) = return $ Just (t, BL.append (frmData f) x)
        -- zero opcode only tolerated if data type was sent earlier
        fragmentedCont
          | Just t <- dataT = fragmented t
          | otherwise = E.throw InvalidOpcode
               
recvClose :: WebSocket -> IO ()
recvClose s = go =<< consume_ s (frameP 0x1000) where
  go f | frmOp f == Close = return ()
       | otherwise        = recvClose s

-- | send a frame
sendFrame :: WebSocket -> AppDataFrame -> IO ()
sendFrame s (typ,buf) =
  sendFrame' s $ Frame True (App typ) buf

sendFrame' :: WebSocket -> Frame -> IO ()
sendFrame' s
  = sendAll' s . serialise

sendPong  s = sendFrame' s . framePong
sendClose s = sendFrame' s frameClose

framePong ping = ping { frmOp = Pong }
frameClose     = Frame True Close BL.empty

-- | parsing monad
newtype Parse e s a
      = Parse (s -> (Either e a, s))
        
instance Functor (Parse e s) where
  fmap f g = return . f =<< g
  
instance Applicative (Parse e s) where
  pure    = return
  f <*> x = ( <$> x ) =<< f

instance Monad (Parse e s) where
  return x = Parse $ \s -> ( Right x, s )
  (Parse f) >>= g' = Parse $ \s ->
    case f s of
      (Left  e, s') -> (Left e, s')
      (Right x, s') -> let Parse g = g' x in g s'

raise :: e -> Parse e s a
raise e = Parse $ \s -> ( Left e, s )

parse :: s -> Parse e s a -> (Either e a, s)
parse s (Parse f) = f s
  
consume :: WebSocket -> P a -> IO (Either WebSocketErr a)
consume s p
  = do modifyMVar (buffer s) $ \b -> 
         let (v, b') = parse b p in
         return (b', v)

consume_ :: WebSocket -> P a -> IO a
consume_ s p
  = from =<< consume s p
    where
      from (Left ex) = E.throw ex
      from (Right v) = return v

-- | frame parse/serialise
serialise :: Frame -> BL.ByteString
serialise f
  = b `BL.cons` bodylenCoded `BL.append` body
  where
    b = 0x80 .|. (opcode $ frmOp f)
    opcode Cont  = 0
    opcode Close = 8
    opcode Ping  = 9
    opcode Pong  = 10
    opcode (App Text) = 1
    opcode (App Binary) = 2
    body = frmData f
    bodylen :: (Integral a) => a
    bodylen = fromIntegral $ BL.length body
    bodylenCoded
      | bodylen <= 125   = encode $ ( bodylen :: Word8 )
      | bodylen <= 65535 = BL.singleton 126 `BL.append` (encode ( bodylen :: Word16 ))
      | otherwise        = BL.singleton 127 `BL.append` (encode ( bodylen :: Word64 ))

maskKey :: P Word32
maskKey = decode <$> payloadP 4

mapi :: ((Int,Word8) -> Word8) -> BL.ByteString -> BL.ByteString
mapi f
  = BL.fromChunks . go 0 . BL.toChunks
  where
    go :: Int -> [B.ByteString] -> [B.ByteString]
    go i [] = []
    go i (c:cs) = B.pack (map f (zip [i..] (B.unpack c)))
                : go (i + B.length c) cs
                  
    
masked :: Word32 -> Int64 -> P a -> P a
masked !key !len (Parse p)
  = Parse (p . xform)
    where
      xform b = let (x,xs) = BL.splitAt len b in go x `BL.append` xs
      go b = mapi f b where
        f (i,b) = b `xor` fromIntegral ((key `shiftR` off) .&. 0xFF) where off = (3 - i `mod` 4) * 8

frameP :: Int -> P Frame
frameP maxLen = 
    do b <- byteP
       let fin  = (b .&. 0x80) /= 0
       op  <- opcode (b .&. 0x0F)
       (masked,len) <- payloadLenP
       when ( len > fromIntegral maxLen ) $ raise (PayloadTooLong len)
       -- control frames have max len of 125 and cannot be fragmented
       when ( isControlOp op && (len > 125 || not fin) ) $ raise InvalidControlFrame
       
       unmask masked len $ 
         Frame <$> pure fin
               <*> pure op
               <*> payloadP len
    where
      opcode 0 = return Cont
      opcode 8 = return Close
      opcode 9 = return Ping
      opcode 10 = return Pong
      opcode 1 = return (App Text)
      opcode 2 = return (App Binary)
      opcode _ = raise InvalidOpcode
  
      unmask m len p
        | not m     = p
        | otherwise = maskKey >>= \key -> masked key len p
             
payloadLenP :: P (Bool,Int64)
payloadLenP
  = maskbit_and_b =<< byteP
    where 
      maskbit_and_b x = (,) <$> pure ((x .&. 0x80) /= 0)
                            <*> max 0 <$> b (x .&. 0x7F)
      b x | x <= 125 = return $ fromIntegral x
          | x == 126 = fromIntegral <$> get16
          | x == 127 = fromIntegral <$> get64
          | otherwise = raise InvalidPayloadLen
      get64 = (decode :: BL.ByteString -> Word64) <$> payloadP 8
      get16 = (decode :: BL.ByteString -> Word16) <$> payloadP 2

payloadP :: Int64 -> P BL.ByteString
payloadP n = Parse $ \s ->
  case BL.splitAt n s of
    (x,xs) | BL.length x == n -> (Right x, xs)
           | otherwise        -> (Left PayloadTooShort, xs)
  
-- | simplistic http
data HttpReq
   = HttpReq [HttpHeader]
     deriving (Eq,Show)
type HttpHeader = (String, String)
data Stop = Stop

-- | handshakde request/response parse
data Request
   = Request { reqHost :: String
             , reqKey :: String }
     deriving (Eq,Show)

data Response
   = Response { respAccept :: String
              , respProtocol :: String }
     deriving (Eq,Show)
              
type P = Parse WebSocketErr BL.ByteString

stripL = snd . span isSpace
strip  = reverse . stripL . reverse . stripL

remEOL :: BL.ByteString -> BL.ByteString
remEOL x | BL.pack [13,10] `BL.isSuffixOf` x = BL.take (BL.length x - 2) x
         | BL.pack [13]    `BL.isSuffixOf` x = BL.take (BL.length x - 1) x
         | BL.pack [10]    `BL.isSuffixOf` x = BL.take (BL.length x - 1) x
         | otherwise                         = x

byteP :: P Word8
byteP = Parse $ \s ->
  case () of
    _ | BL.null s -> (Left ExpectedByte, BL.empty)
      | otherwise -> (Right (BL.head s), BL.tail s)
  
limitedLineP :: Int -> P BL.ByteString
limitedLineP n = go n BL.empty where
  go 0 l = raise LineTooLong
  go n l = from =<< byteP where
    from 10 = return $ remEOL l
    from  x = go (n-1) (l `BL.snoc` x)

lineP = limitedLineP 256

hdrP :: P (Either Stop HttpHeader)
hdrP
  = from =<< lineP
  where
    from x
      | BL.null x = return $ Left Stop
      | (l, r) <- UTF8.break (==':') x, not (BL.null r) = return $ Right (mkhdr l $ BL.tail r)
      | otherwise = raise ExpectedHttpHdr
    mkhdr k' v'
      = let k = map toLower $ UTF8.toString k'
            v = UTF8.toString v' in
        (,) (strip k) (strip v)
  
reqP :: P Request
reqP = make =<< caption *> (HttpReq <$> headers [] 100) where
  make (HttpReq r)
    = from ( Request <$> lookup "host" r
                 <*> lookup "sec-websocket-key" r )
      where from Nothing  = raise InvalidGetRequest
            from (Just r) = pure r
         
  caption
    = from_words . words . UTF8.toString <$> lineP
      where from_words ("GET":"/chat":_) = pure ()
            from_words _ = raise InvalidGetRequest
  headers xs 0 = raise TooManyHttpHeaders
  headers xs n =
    do h <- hdrP
       case h of
         Left Stop -> return (reverse xs)
         Right h   -> headers (h:xs) (n-1)

response :: Request -> Response
response r
  = Response { respProtocol = "chat"
             , respAccept = base64 . sha1 $ key }
    where key = reqKey r ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

httpResponse :: Response -> BL.ByteString
httpResponse r
  = UTF8.fromString $
    "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: " ++ respAccept r ++ "\r\n\r\n"

-- digest/encode helpers
-- TODO: drop dep on openssl maybe
sha1 :: String -> [Word8]
sha1 x = unsafePerformIO (from <$> readProcessWithExitCode "openssl" ["dgst", "-sha1"] x)
  where from (ExitFailure _, _, _) = error "sha1 digest failure"
        from (_, stdout, _) = bytes $ head $ reverse $ words $ head $ lines stdout
        bytes [] = []
        bytes (a:b:xs) = byte a b : bytes xs
        bytes _ = error "bad sha1 output"
        byte h l = v h * 16 + v l
        v x | ord x >= ord '0' && ord x <= ord '9' = fromIntegral $ ord x - ord '0'
            | ord x >= ord 'a' && ord x <= ord 'f' = fromIntegral $ 10 + ord x - ord 'a'
            | ord x >= ord 'A' && ord x <= ord 'F' = fromIntegral $ 10 + ord x - ord 'A'
            | otherwise = error "bad sha1 output"

base64 :: [Word8] -> String
base64 = aux
  where
    aux [] = []
    aux [a] = enc1 (fromIntegral a)
    aux [a,b] = enc2 $ (0x100 * fromIntegral a) + (fromIntegral b)
    aux (a:b:c:xs) = enc3 ((0x10000 * fromIntegral a) + (0x100 * fromIntegral b) + (fromIntegral c))
                     ++ base64 xs
                     
    enc1, enc2, enc3 :: Word32 -> String
    enc1 x = cut x 2 : cut (x `shiftL` 6) 0 : '=' : '=' : []
    enc2 x = cut x 10 : cut x 4 : cut (x `shiftL` 2) 0 : '=' : []
    enc3 x = cut x 18 : cut x 12 : cut x 6 : cut x 0 : []
    
    cut :: Word32 -> Int -> Char
    cut x n = tab' $ (x `shiftR` n) .&. 63
    tab' = tab . fromIntegral
    tab v | v >= 0  && v <= 25 = chr $ v + ord 'A'
          | v >= 26 && v <= 51 = chr $ (v-26) + ord 'a'
          | v >= 52 && v <= 61 = chr $ (v-52) + ord '0'
          | v == 62 = '+'
          | v == 63 = '/'
          | otherwise = error "impossible"

-- send/recv helpers
bufferSz = 4096
data StreamErr = EOF deriving Typeable
instance E.Exception StreamErr
instance Show StreamErr

incoming :: (Int -> IO B.ByteString) -> IO BL.ByteString
incoming recv =
  (BL.fromChunks <$> chunks) `E.catch` err
  where
    chunks
      = do ch <- recv bufferSz
           if B.null ch
              then return [] -- EOF
              else ((:) <$> pure ch <*> unsafeInterleaveIO chunks)
    err :: E.SomeException -> IO BL.ByteString
    err x = return BL.empty

sendAll :: WebSocket -> B.ByteString -> IO ()
sendAll s buf =
    send s buf >>= \sent ->
       when (sent < B.length buf) $
               sendAll s (B.drop sent buf)

sendAll' :: WebSocket -> BL.ByteString -> IO ()
sendAll' s = mapM_ (sendAll s) . BL.toChunks
