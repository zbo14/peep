module Conn
(
    Conn,
    Conn.new,
    amChoked,
    amInterested,
    remoteChoked,
    remoteInterested,
    recv',
    recvRoutine,
    handleRoutine,
    sendRoutine,
    blocks,
    requests,
    keepAlive,
    block,
    choke,
    unchoke,
    interested,
    notInterested,
    bitfield,
    have,
    requestBlock,
    requestPiece,
    remoteHas
) where 

import Data.Set as S
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (Builder,lazyByteString)
import Chunk (bytes,index)
import Producer (one)

import Block (Block)
import qualified Bitfield as BF
import Const (blockSize)
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Except
import Contact (Contact,fromInfo)
import Id (Id,bytes)
import Info (Info,bytes,addr,host,port)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy (recv,sendAll)
import BigEndian(encodeBytes,decodeBytes,decodeBuilder)
import Util (ExceptIO,ExceptSTM,bTake,exceptIO,exceptSTM,now)

msgKeepAlive = encodeBytes BS.empty
msgChoke = encodeBytes (fromIntegral $ fromEnum Choke :: Word)
msgUnchoke = encodeBytes (fromIntegral $ fromEnum Unchoke :: Word)
msgInterested = encodeBytes (fromIntegral $ fromEnum Interested :: Word)
msgNotInterested = encodeBytes (fromIntegral $ fromEnum NotInterested :: Word)

data Conn = Conn {
    contact :: Contact,
    socket :: Socket,
    amChoked :: TVar Bool,
    amInterested :: TVar Bool,
    remoteChoked :: TVar Bool,
    remoteInterested :: TVar Bool,
    numPieces :: Int,
    opened :: Int,
    blocks :: TChan (Int,Block),
    requests :: TChan (Int,Int),
    requested :: TVar (S.Set (Int,Int)),
    inbox :: TChan BS.ByteString,
    outbox :: TChan BS.ByteString,
    remoteBitfield :: TVar BF.Bitfield
}

data MsgType = 
    Choke 
    | Unchoke 
    | Interested 
    | NotInterested 
    | Have 
    | Bitfield 
    | Request 
    | Block 
    | Cancel 
    | Port 
    | Rate deriving (Enum)

instance Id Conn where 
    bytes = Id.bytes . contact

instance Info Conn where
    bytes = Info.bytes . contact 
    addr = Info.addr . contact 
    host = Info.host . contact 
    port = Info.port . contact

instance Eq Conn where 
    Conn{contact=c1} == Conn{contact=c2} = c1 == c2

instance Show Conn where 
    show Conn{contact=c,opened=o} =
        "CONN {" ++ show c ++ ", OPENED=" ++ show o ++ "}"

instance Ord Conn where 
    Conn{contact=c1} `compare` Conn{contact=c2} = c1 `compare` c2

new :: Info a => a -> Socket -> Int -> IO Conn
new i s np = do
    tm <- now
    ac <- newTVarIO True
    ai <- newTVarIO False
    rc <- newTVarIO True
    ri <- newTVarIO False
    bf <- newTVarIO $ BF.empty np
    bls <- newTChanIO
    rqs <- newTChanIO
    rqd <- newTVarIO S.empty
    inb <- newTChanIO
    out <- newTChanIO
    return Conn {
        contact=fromInfo i,
        socket=s,
        amChoked=ac,
        amInterested=ai,
        remoteChoked=rc,
        remoteInterested=ri,
        numPieces=np,
        opened=tm,
        blocks=bls,
        requests=rqs,
        requested=rqd,
        inbox=inb,
        outbox=out,
        remoteBitfield=bf
    }

recv' :: Socket -> ExceptIO Builder
recv' s = ExceptT $ recv s 4096 >>= \b -> 
    if BS.length b == 0 
        then pure $ Left "dead connection" 
        else pure . Right $ lazyByteString b

remoteHas :: Conn -> Int -> IO Bool 
remoteHas Conn{remoteBitfield=tv} pi = readTVarIO tv >>= return . BF.has pi

readMessage :: Conn -> Socket -> Builder -> ExceptIO (BS.ByteString,Builder) 
readMessage cn s q = case decodeBuilder q of 
    Just (b,bd) -> pure (b,bd)
    otherwise -> recv' s >>= readMessage cn s . (q `mappend`)

recvRoutine :: Conn -> Builder -> ExceptIO ()
recvRoutine cn q = recving q
    where s = socket cn
          inb = inbox cn
          recving q = do
            (msg,q') <- readMessage cn s q
            exceptIO . atomically $ writeTChan inb msg
            recving q'

handleRoutine :: Conn -> IO ()
handleRoutine cn = 
    forever $ do 
        m <- atomically (readTChan inb) 
        handleMessage cn m
    where inb = inbox cn

handleMessage :: Conn -> BS.ByteString -> IO ()
handleMessage cn b = case BS.uncons b of 
    Just (ty,m) -> do 
        runExceptT $ handleMessage' cn (toEnum $ fromIntegral ty) m
        return ()
    otherwise -> return ()

decodeBlockIndex :: BS.ByteString -> ExceptIO (Int,BS.ByteString)
decodeBlockIndex b = case decodeBytes b of 
    Just (bi,b') -> pure (bi,b')
    otherwise -> exceptIO $ Left "could not decode block index"

decodePieceIndex :: BS.ByteString -> ExceptIO (Int,BS.ByteString)
decodePieceIndex b = case decodeBytes b of 
    Just (pi,b') -> pure (pi,b')
    otherwise -> exceptIO $ Left "could not decode piece index"

wasRequested :: Int -> Int -> TVar (S.Set (Int,Int)) -> STM (Either String ())
wasRequested pi bi tv = readTVar tv >>= \rqd -> if S.member (pi,bi) rqd 
    then writeTVar tv (S.delete (pi,bi) rqd) >>= \_ -> pure $ Right ()
    else return $ Left "block was not requested"

handleMessage' :: Conn -> MsgType -> BS.ByteString -> ExceptIO ()
handleMessage' cn Choke _ =
    exceptIO . atomically $ modifyTVar (amChoked cn) (\_ -> True) 
handleMessage' cn Unchoke _ = 
    exceptIO . atomically $ modifyTVar (amChoked cn) (\_ -> False) 
handleMessage' cn Interested _ = 
    exceptIO . atomically $ modifyTVar (remoteInterested cn) (\_ -> True) 
handleMessage' cn NotInterested _ = 
    exceptIO . atomically $ modifyTVar (remoteInterested cn) (\_ -> False) 
handleMessage' cn Have b = do 
    (pi,_) <- decodePieceIndex b
    exceptIO . atomically $ modifyTVar (remoteBitfield cn) (snd . runState (BF.add pi))
handleMessage' Conn{numPieces=np,remoteBitfield=tv} Bitfield b =
    case BF.withBytes np b of 
        Just bf -> exceptIO . atomically $ writeTVar tv bf
        otherwise -> exceptIO $ Left "could not set bitfield"
handleMessage' cn@Conn{requested=tv} Block b = do
    (pi,b1) <- decodePieceIndex b
    (bi,b2) <- decodeBlockIndex b1
    exceptIO $ wasRequested pi bi tv
    let bl = one (bTake blockSize b2) bi
    exceptIO . atomically $ writeTChan (blocks cn) (pi,bl)
handleMessage' cn Request b = do 
    (pi,b1) <- decodePieceIndex b
    (bi,_) <- decodeBlockIndex b1
    exceptIO . atomically $ writeTChan (requests cn) (pi,bi)
handleMessage' _ _ _ = pure ()

sendRoutine :: Conn -> IO ()
sendRoutine Conn{outbox=out,socket=s} = forever $ atomically (readTChan out) >>= sendAll s

keepAlive :: Conn -> STM ()
keepAlive Conn{outbox=out} = writeTChan out msgKeepAlive

choke :: Conn -> STM (Either String ())
choke Conn{remoteChoked=rc,outbox=out} = 
    readTVar rc >>= \rc' -> if rc' 
        then return $ Left "remote already choked"  
        else do 
            writeTVar rc True
            writeTChan out msgChoke
            pure $ Right ()

unchoke :: Conn -> STM (Either String ())
unchoke Conn{remoteChoked=rc,outbox=out} = 
    readTVar rc >>= \rc' -> if not rc' 
        then return $ Left "remote already unchoked"  
        else do 
            writeTVar rc False
            writeTChan out msgUnchoke
            pure $ Right ()

interested :: Conn -> STM (Either String ())
interested Conn{amInterested=ai,outbox=out} =
    readTVar ai >>= \ai' -> if ai' 
        then return $ Left "already interested"
        else do 
            writeTVar ai True
            writeTChan out msgInterested
            pure $ Right ()

notInterested :: Conn -> STM (Either String ())
notInterested Conn{amInterested=ai,outbox=out} = 
    readTVar ai >>= \ai' -> if not ai' 
        then return $ Left "already not interested"
        else do
            writeTVar ai False
            writeTChan out msgNotInterested
            pure $ Right () 

have :: Conn -> Int -> STM (Either String ())
have Conn{numPieces=np} pi | pi < 0 || pi >= np = return $ Left "invalid piece index"
have Conn{outbox=out} pi = 
    writeTChan out msgHave >>= \_ -> pure $ Right ()
    where pi' = encodeBytes pi
          pre = BS.cons . fromIntegral $ fromEnum Have
          msgHave = encodeBytes $ pre pi'

canRequest :: Conn -> STM (Either String ())
canRequest Conn{amChoked=ac,amInterested=ai} = 
    readTVar ac >>= \ac' -> if ac' 
        then return $ Left "cannot request when choked"
        else readTVar ai >>= \ai' -> if not ai' 
            then return $ Left "cannot request when not interested"
            else pure $ Right ()

requestBlock :: Conn -> Int -> Int -> ExceptSTM ()
requestBlock cn pi bi = ExceptT (canRequest cn) >>= \_ -> exceptSTM $ requestBlock' cn pi bi 

requestBlock' :: Conn -> Int -> Int -> STM ()
requestBlock' Conn{outbox=out,requested=tv} pi bi =
    modifyTVar tv (S.insert (pi,bi)) >>= \_ -> writeTChan out msgRequest
    where pi' = encodeBytes pi
          bi' = encodeBytes bi
          pre = BS.cons . fromIntegral $ fromEnum Request
          msgRequest = encodeBytes . pre $ pi' <> bi'

requestPiece :: Conn -> Int -> Int -> ExceptSTM ()
requestPiece cn pi sz = ExceptT (canRequest cn) >>= \_ -> exceptSTM $ requestPiece' cn pi sz

requestPiece' :: Conn -> Int -> Int -> STM ()
requestPiece' cn pi sz = fn 0
    where dv = sz `div` blockSize
          nb = if sz `mod` blockSize == 0 then dv else dv+1
          fn bi | bi == nb = pure ()
          fn bi = requestBlock' cn pi bi >>= \_ -> fn (bi+1)

canBlock :: Conn -> STM (Either String ())
canBlock Conn{remoteChoked=rc,remoteInterested=ri} = 
    readTVar rc >>= \rc' -> if rc' 
        then return $ Left "cannot send block when remote choked"
        else readTVar ri  >>= \ri' -> if not ri' 
            then return $ Left "cannot send block when remote not interested"
            else pure $ Right () 

block :: Conn -> Int -> Block -> ExceptSTM ()
block cn@Conn{outbox=out,requested=rqd} pi bl =
    ExceptT (canBlock cn) >>= \_ -> exceptSTM $ writeTChan out msgBlock
    where bi = Chunk.index bl
          pi' = encodeBytes pi 
          bi' = encodeBytes bi
          by = Chunk.bytes bl
          pre = BS.cons . fromIntegral $ fromEnum Block
          msgBlock = encodeBytes . pre $ mconcat [pi',bi',by]

bitfield :: Conn -> BF.Bitfield -> STM ()
bitfield Conn{outbox=out} bf =
    writeTChan out msgBitfield
    where pre = BS.cons . fromIntegral $ fromEnum Bitfield
          msgBitfield = encodeBytes . pre $ BF.bytes bf