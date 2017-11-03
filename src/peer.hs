module Peer
(
    Peer,
    Peer.new,
    lookupConns,
    lookupFile,
    lookupPiece,
    lookupBlock,
    conns,
    files,
    acceptRoutine,
    connect,
    serve,
    Peer.torrent,
    Peer.requestPiece,
    requestFile
)
where 

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import Control.Monad.State

import qualified Chunk as CH
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS 
import Data.ByteString.Builder (Builder,lazyByteString,toLazyByteString)
import qualified Collection as C
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Network.Socket as N

import Block (Block)
import Const (protocolName,numReservedBytes,idSize)
import Conn (Conn,block,have,blocks,requests,new,recv',recvRoutine,handleRoutine,sendRoutine,requestPiece,remoteHas)
import Contact (Contact,fromInfo)
import Collection ()
import File (File,pieces)
import Id (Id,bytes)
import IdBytes (IdBytes,new)
import Info (Info,bytes,addr,host,port)
import InfoBytes (fromAddrId)
import Torrent (new,torrent)
import Network.Socket.ByteString.Lazy (sendAll)
import Piece (Piece)
import Util (ExceptIO,ExceptSTM,toExceptIO,exceptIO,exceptSTM,bSplitAt,bLength)
import Value (Dict)
import Whole (isComplete)

beginHandshake = BS.cons (fromIntegral $ length protocolName) $ C8.pack protocolName
lenBeginHandshake = bLength beginHandshake
reservedBytes = BS.replicate (fromIntegral numReservedBytes) 0

type Conns = TVar (M.Map BS.ByteString [Conn])
type Files = TVar (M.Map BS.ByteString File)

data Peer = Peer {
    contact :: Contact,
    conns :: Conns,
    -- errors :: TChan String,
    files :: Files,
    socket :: N.Socket
}

instance Info Peer where 
    bytes = Info.bytes . contact
    addr = Info.addr . contact 
    host = Info.host . contact 
    port = Info.port . contact

instance Id Peer where
    bytes = Id.bytes . contact

new :: Info a => a -> IO Peer
new i = do 
    let c = fromInfo i
    ctv <- newTVarIO M.empty
    ftv <- newTVarIO M.empty
    s <- N.socket N.AF_INET N.Stream N.defaultProtocol
    N.bind s (Info.addr i)
    return Peer {
        contact=c,
        conns=ctv,
        files=ftv,
        socket=s
    }

acceptRoutine :: Peer -> IO ()
acceptRoutine p = do
    N.listen s 1
    forever . runExceptT $ handleConn p s
    where s = socket p

lookupFile :: Id a => a -> Files -> STM (Either String File)
lookupFile h tv =
    readTVar tv >>= \fm -> case M.lookup (Id.bytes h) fm of 
        Just f -> return $ Right f
        Nothing -> return $ Left "cannot find file"

lookupPiece :: Id a => a -> Int -> Files -> ExceptSTM (Piece,File)
lookupPiece h pi tv = ExceptT (lookupFile h tv) >>= \f -> case C.lookup pi f of 
    Just pc -> pure (pc,f) 
    otherwise -> exceptSTM $ Left "cannot find piece"

lookupBlock :: Id a => a -> Int -> Int -> Files -> ExceptSTM (Block,Piece,File)
lookupBlock h pi bi tv = lookupPiece h pi tv >>= \(pc,f) -> case C.lookup bi pc of 
    Just bl -> pure (bl,pc,f) 
    otherwise -> exceptSTM $ Left "cannot find block"

updateFile :: Id a => a -> File -> Files -> STM File
updateFile h f tv = (modifyTVar tv $ M.insert (Id.bytes h) f) >>= \_ -> return f

updatePiece :: Id a => Piece -> a -> File -> Files -> STM (Either String (Piece,File))
updatePiece pc h f tv = let (res,f') = runState (C.update pc) f in case res of 
    C.None -> return $ Left "cannot add piece to file"
    otherwise -> updateFile h f' tv >>= \f -> return $ Right (pc,f)

updateBlock :: Id a => Block -> a -> Int -> Files -> ExceptSTM (Block,Piece,File)
updateBlock bl h pi tv = lookupPiece h pi tv >>= \(pc,f) -> let (res,pc') = runState (C.update bl) pc in case res of
    C.None -> exceptSTM $ Left "cannot add block to piece" 
    otherwise -> ExceptT (updatePiece pc' h f tv) >>= \(pc,f) -> pure (bl,pc,f)

lookupConns :: Id a => a -> Conns -> ExceptIO [Conn]
lookupConns h tv =
    ExceptT $ readTVarIO tv >>= \cm -> case M.lookup (Id.bytes h) cm of
        Just cns -> return $ Right cns
        otherwise -> return $ Left "cannot find conns"

insertConns :: Id a => a -> [Conn] -> Conns -> IO () 
insertConns h cns tv = 
    atomically $ modifyTVar tv $ M.insert (Id.bytes h) cns

blockRoutine :: Id a => Files -> Conn -> a -> ExceptIO ()
blockRoutine tv cn h = let bls = Conn.blocks cn in forever $ do
    (pi,bl) <- exceptIO . atomically $ readTChan bls
    (_,pc,_) <- exceptIO $ updateBlock bl h pi tv
    if isComplete pc 
        then ExceptT . atomically $ have cn pi 
        else pure ()

requestRoutine :: Id a => Files -> Conn -> a -> ExceptIO ()
requestRoutine tv cn h = let rqs = requests cn in forever $ do 
    (pi,bi) <- exceptIO . atomically $ readTChan rqs
    (bl,_,_) <- exceptIO $ lookupBlock h pi bi tv
    exceptIO $ block cn pi bl

connRoutines :: Id a => Peer -> Conn -> a -> Builder -> IO ()
connRoutines Peer{files=tv} cn h q = do 
    forkIO $ runExceptT (recvRoutine cn q) >>= \_ -> return ()
    forkIO $ handleRoutine cn
    forkIO $ sendRoutine cn
    forkIO $ runExceptT (blockRoutine tv cn h) >>= \_ -> return ()
    forkIO $ runExceptT (requestRoutine tv cn h) >>= \_ -> return ()
    return ()

handleConn :: Peer -> N.Socket -> ExceptIO ()
handleConn p s = do 
    (s',a) <- exceptIO $ N.accept s
    (hb,ib,q) <- handleClientHandshake "begin" s'
    h <- toExceptIO "could not get hash" $ IdBytes.new hb
    i <- toExceptIO "could not get id" $ IdBytes.new ib
    let ftv = files p
    f <- ExceptT . atomically $ lookupFile h ftv
    let ctv = conns p
    cns <- lookupConns h ctv
    let pl = BS.concat [beginHandshake,reservedBytes,hb,Id.bytes p]
    exceptIO $ sendAll s' pl
    let ib = fromAddrId a i
    cn <- exceptIO $ Conn.new ib s' . length $ pieces f 
    exceptIO $ insertConns h (cn:cns) ctv
    exceptIO $ connRoutines p cn h q

type Handshake = (BS.ByteString,BS.ByteString,Builder)

requestPiece' :: Bool -> [Conn] -> Int -> Int -> ExceptIO Bool
requestPiece' tf [] _ _ = pure tf
requestPiece' tf (hd:tl) pi sz = do 
    exceptIO (remoteHas hd pi) >>= \has -> if has 
        then (exceptIO (Conn.requestPiece hd pi sz) >>= \_ -> pure True) <|> requestPiece' False tl pi sz
        else requestPiece' False tl pi sz

requestPiece :: Id a => Peer -> a -> Int -> ExceptIO Bool
requestPiece Peer{conns=ctv,files=ftv} h pi = do
    (pc,_) <- exceptIO $ lookupPiece h pi ftv
    cns <- lookupConns h ctv
    requestPiece' False cns pi $ CH.size pc 

requestFile :: Id a => Peer -> a -> ExceptIO (TChan (Int,Bool))
requestFile Peer{conns=ctv,files=ftv} h = do 
    f <- ExceptT . atomically $ lookupFile h ftv
    cns <- lookupConns h ctv
    tch <- exceptIO newTChanIO
    exceptIO $ requestFile' tch cns $ pieces f

requestFile' :: TChan (Int,Bool) -> [Conn] -> [Piece] -> IO (TChan (Int,Bool))
requestFile' tch _ [] = pure tch
requestFile' tch cns (hd:tl) = do 
    let pi = CH.index hd
    forkIO $ runExceptT (requestPiece' False cns pi $ CH.size hd) >>= \eit -> case eit of 
        Right res -> atomically $ writeTChan tch (pi,res)
        otherwise -> pure ()
    requestFile' tch cns tl

connect :: (Info a, Id b) => Peer -> a -> b -> ExceptIO Conn
connect p i h = do
    let hb = Id.bytes h
        ftv = files p
    f <- ExceptT . atomically $ lookupFile h ftv
    let ctv = conns p
    cns <- lookupConns h ctv
    s <- exceptIO $ N.socket N.AF_INET N.Stream N.defaultProtocol
    exceptIO . N.connect s $ Info.addr i
    let pl = BS.concat [beginHandshake,reservedBytes,hb,Id.bytes p]
    exceptIO $ sendAll s pl
    (_,_,q) <- handleServerHandshake (hb,Id.bytes i,lazyByteString BS.empty) "begin" s
    cn <- exceptIO . Conn.new (fromInfo i) s . length $ pieces f 
    exceptIO $ insertConns h (cn:cns) ctv
    exceptIO $ connRoutines p cn h q
    return cn

serve :: Info a => Peer -> BS.ByteString -> String -> [a] -> IO Dict
serve p by nm nds = do 
    let (f,d,h) = Torrent.new by nm nds 
    atomically $ updateFile h f $ files p
    insertConns h [] $ conns p
    return d

torrent :: Peer -> Dict -> ExceptIO IdBytes
torrent p d = do
    (f,h) <- exceptIO $ Torrent.torrent d 
    exceptIO $ updateFile h f $ files p
    exceptIO $ insertConns h [] $ conns p
    return h

handleServerHandshake :: Handshake -> String -> N.Socket -> ExceptIO Handshake
handleServerHandshake (h,i,q) "begin" s = 
    let b = toLazyByteString q in if bLength b < 20
        then recv' s >>= \q' -> handleServerHandshake (h,i,q <> q') "begin" s
        else let (b1,b2) = bSplitAt 20 b in if b1 /= beginHandshake
            then exceptIO $ Left "server handshake failed at begin stage"
            else handleServerHandshake (h,i,lazyByteString b2) "reserved" s

handleServerHandshake (h,i,q) "reserved" s =
    let b = toLazyByteString q in if bLength b < 8
        then recv' s >>= \q' -> handleServerHandshake (h,i,q <> q') "reserved" s 
        else let (b1,b2) = bSplitAt 8 b in if b1 /= reservedBytes
            then exceptIO $ Left "server handshake failed at reserved-bytes stage"
            else handleServerHandshake (h,i,lazyByteString b2) "hash" s

handleServerHandshake (h,i,q) "hash" s =
    let b = toLazyByteString q in if bLength b < idSize 
        then recv' s >>= \q' -> handleServerHandshake (h,i,q <> q') "hash" s
        else let (b1,b2) = bSplitAt idSize b in if b1 /= h
            then exceptIO $ Left "server handshake failed at hash stage"
            else handleServerHandshake (h,i,lazyByteString b2) "id" s 

handleServerHandshake (h,i,q) "id" s = 
    let b = toLazyByteString q in if bLength b < idSize 
        then recv' s >>= \q' -> handleServerHandshake (h,i,q <> q') "id" s
        else let (b1,b2) = bSplitAt idSize b in if b1 /= i
            then exceptIO $ Left "server handshake failed at id stage"
            else pure (h,i,lazyByteString b2)

handleServerHandshake _ _ _ = exceptIO $ Left "unexpected pattern for server handshake" 

handleClientHandshake :: String -> N.Socket -> ExceptIO Handshake
handleClientHandshake = handleClientHandshake' (BS.empty,BS.empty,lazyByteString BS.empty)

handleClientHandshake' :: Handshake -> String -> N.Socket -> ExceptIO Handshake
handleClientHandshake' (h,i,q) "begin" s = 
    let b = toLazyByteString q in if bLength b < lenBeginHandshake
        then recv' s >>= \q' -> handleClientHandshake' (h,i,q <> q') "begin" s
        else let (b1,b2) = bSplitAt lenBeginHandshake b in if b1 /= beginHandshake
            then exceptIO $ Left "client handshake failed at begin stage"
            else handleClientHandshake' (h,i,lazyByteString b2) "reserved" s

handleClientHandshake' (h,i,q) "reserved" s =
    let b = toLazyByteString q in if bLength b < numReservedBytes
        then recv' s >>= \q' -> handleClientHandshake' (h,i,q <> q') "reserved" s
        else let (b1,b2) = bSplitAt numReservedBytes b in if b1 /= reservedBytes
            then exceptIO $ Left "client handshake failed at reserved-bytes stage"
            else handleClientHandshake' (h,i,lazyByteString b2) "hash" s

handleClientHandshake' (h,i,q) "hash" s =
    let b = toLazyByteString q in if bLength b < idSize 
        then recv' s >>= \q' -> handleClientHandshake' (h,i,q <> q') "hash" s
        else let (b1,b2) = bSplitAt idSize b in
            handleClientHandshake' (b1,i,lazyByteString b2) "id" s

handleClientHandshake' (h,i,q) "id" s = 
    let b = toLazyByteString q in if bLength b < idSize 
        then recv' s >>= \q' -> handleClientHandshake' (h,i,q <> q') "id" s 
        else let (b1,b2) = bSplitAt idSize b in
            pure (h,b1,lazyByteString b2)

handleClientHandshake' _ _ _ = exceptIO $ Left "unexpected pattern for client handshake" 



