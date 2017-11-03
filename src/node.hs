module Node
(
    Node,
    Node.new,
    ping,
    findNode, 
    contact,
    router,
    recvRoutine,
    readMessage,
    iterFindNode,
    stop
) where 

import Control.Concurrent
import Control.Concurrent.MVar (MVar,newEmptyMVar)
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Bucket (Bucket,contacts,low,range,updated,withContacts)
import Binary (add,mod)
import Const (bucketTimeout,messageTimeout,nodeTimeout,recvSize,refreshInterval)
import Data.ByteString.Lazy (ByteString,fromStrict,toStrict)
import Data.ByteString.Lazy.Char8 (pack)
import Contact (Contact,addr,fromInfo,lastHeard,isBad,bad,notBad)
import Data.Char (chr)
import Data.List (sortBy)
import Message (Tuple,response,queryPing,queryFindNode,messageToTuple)
import Network.Socket.ByteString (recvFrom,sendAllTo)
import Control.Applicative ((<|>))
import System.Random (randomRs,getStdGen)
import Bencode (encode,decode)
import Value (List,Dict,Value(B,L),emptyDict,newDict,dictLookup,wrap,unwrap)
import Id (Id,bytes,cmp,eq,sort)
import IdBytes (IdBytes,new,rand)
import Info (Info,bytes,addr,port,host)
import InfoBytes (InfoBytes,new,fromAddrId)
import Util (ExceptIO,MaybeIO,exceptIO,maybeIO,toExceptIO,now)
import Data.Either.Utils (maybeToEither)

import qualified Data.Map as M
import qualified Network.Socket as N
import qualified Routing as R

type Messages = TVar (M.Map ByteString (MVar (Maybe Dict)))
type Router = TVar [Bucket]

data Node = Node {
    shutdown :: MVar (),
    contact :: Contact,    
    router :: Router,
    socket :: N.Socket,
    messages :: Messages
}

instance Info Node where 
    bytes = Info.bytes . contact
    addr = Info.addr . contact 
    port = Info.port . contact 
    host = Info.host . contact

instance Id Node where
    bytes = Id.bytes . contact

stop :: Node -> IO ()
stop Node{shutdown=tv} = do
    putStrLn "stopping node"
    putMVar tv ()

txid :: IO ByteString
txid = getStdGen >>= return . pack . map chr . take 2 . randomRs (0x0, 0xFF)

new :: Info a => a -> IO Node 
new i = do
    let c = fromInfo i
    s <- N.socket N.AF_INET N.Datagram N.defaultProtocol
    sh <- newEmptyMVar
    N.bind s (Contact.addr c)
    rtv <- newTVarIO R.new
    mtv <- newTVarIO M.empty
    return Node {
        shutdown=sh,
        contact=c,
        router=rtv,
        socket=s,
        messages=mtv
    }

register :: Messages -> ByteString -> MVar (Maybe Dict) -> STM ()
register tv ti mv = modifyTVar tv $ M.insert ti mv

unregister :: Messages -> ByteString -> STM (Either String (MVar (Maybe Dict)))
unregister tv ti = readTVar tv >>= \m -> case M.lookup ti m of 
    Just mv -> do 
        let m' = M.delete ti m 
        writeTVar tv m' 
        return $ Right mv
    Nothing -> return $ Left "could not find message"

timeoutMessage :: Messages -> ByteString -> IO () 
timeoutMessage tv ti = do
    threadDelay messageTimeout
    res <- atomically $ unregister tv ti
    case res of 
        Right mv -> putMVar mv Nothing
        otherwise -> pure ()

sendAllTo' :: N.Socket -> ByteString -> N.SockAddr -> IO ()
sendAllTo' s b a = sendAllTo s (toStrict b) a

ping :: Info a => Node -> a -> IO (Maybe ())
ping nd@Node{messages=tv,socket=s} i = do 
    t <- txid
    mv <- newEmptyMVar
    let ti = t `mappend` Id.bytes i
        b = encode . wrap $ queryPing nd t
    atomically $ register tv ti mv
    sendAllTo' s b $ Info.addr i
    forkIO $ timeoutMessage tv ti
    takeMVar mv >>= \_ -> pure $ Just ()

findNode :: (Info a,Id b) => Node -> a -> b -> IO (Either String R.Result)
findNode nd@Node{messages=tv,socket=s} i tg = do
    t <- txid
    mv <- newEmptyMVar
    let ti = t `mappend` Id.bytes i
        b = encode . wrap $ queryFindNode tg nd t
    atomically $ register tv ti mv
    sendAllTo' s b $ Info.addr i
    forkIO $ timeoutMessage tv ti
    res <- takeMVar mv 
    case res of 
        Just d -> return $ getResult d
        otherwise -> return $ Left "no response"

parseList :: List -> [InfoBytes] -> Maybe R.Result
parseList [] acc = return . R.Contacts . reverse $ map fromInfo acc
parseList (h:tl) acc = unwrap h >>= InfoBytes.new >>= parseList tl . (:acc)

parseResult :: Value -> Maybe R.Result
parseResult (B b) = InfoBytes.new b >>= return . R.Contact . fromInfo
parseResult (L l) = parseList l []
parseResult _ = Nothing

getResult :: Dict -> Either String R.Result 
getResult d = 
    case dictLookup "nodes" d of
        Just v -> maybeToEither "could not parse result" $ parseResult v
        otherwise ->  Left "could not find result"

goFindNode :: Id a => Node -> ByteString -> a -> ExceptIO R.Result
goFindNode Node{router=tv} t tg = 
    ExceptT $ readTVarIO tv >>= return . maybeToEither "could not find target in routing table" . R.find tg

lookupTarget :: Dict -> Either String IdBytes
lookupTarget d = case dictLookup "target" d of 
    Just v -> maybeToEither "could not get idbytes" $ unwrap v >>= IdBytes.new
    otherwise -> Left "could not find 'target'"

handleMessage :: Node -> N.SockAddr -> Tuple -> ExceptIO InfoBytes
handleMessage nd@Node{socket=s} a ("q","ping",t,i,_) = exceptIO $ do
    sendAllTo' s b a 
    pure $ fromAddrId a i
    where msg = response emptyDict nd t
          b = encode . wrap $ msg
handleMessage nd@Node{socket=s} a ("q","find_node",t,i,d) = do
    tg <- exceptIO $ lookupTarget d
    res <- goFindNode nd t tg
    let wrapper (R.Contact c) = wrap $ Info.bytes c
        wrapper (R.Contacts cs) = wrap $ map (wrap . Info.bytes) cs
        v = wrapper res
        b = encode . wrap $ response (newDict [("nodes",v)]) nd t
    exceptIO $ sendAllTo' s b a >>= \_ -> pure $ fromAddrId a i
handleMessage nd@Node{messages=tv,socket=s} a ("r",_,t,i,d) = do
    mv <- ExceptT . atomically $ unregister tv $ t `mappend` Id.bytes i
    exceptIO $ putMVar mv (pure d) >>= \_ -> pure $ fromAddrId a i

readMessage :: Node -> ExceptIO ()
readMessage nd@Node{socket=s} = do
    (b,a) <- exceptIO $ recvFrom s recvSize
    (v,_,_) <-  maybeToEither "could not decode message" $ decode $ fromStrict b
    m <- maybeToEither "could not unwrap message" $ unwrap v
    tup <- maybeToEither "could not get message tuple" $ messageToTuple m
    handleMessage nd a tup >>= toExceptIO "could not update" . update nd

-- readShutdown :: Node -> IO ()
-- readShutdown Node{shutdown=mv} = do
--     takeMVar mv
--     putStrLn "took mvar"

recvRoutine :: Node -> IO ()
recvRoutine nd@Node{shutdown=mv} = do
    stop <- tryTakeMVar mv
    case stop of 
        Nothing -> do 
            runExceptT $ readMessage nd
            recvRoutine nd
        otherwise -> do
            putStrLn "stopping recv routine"
            pure ()

refreshRoutine :: Node -> IO ()
refreshRoutine nd@Node{router=tv} = 
    forever $ do
        r <- readTVarIO tv 
        sequence $ map refresh r 
        threadDelay refreshInterval
        return ()
    where refresh b = do
            bef <- updated b
            aft <- now
            if aft-bef > bucketTimeout
                then runMaybeT $ refreshBucket nd b
                else return Nothing

-- we should ping node if we haven't heard from it in awhile or if it's bad
shouldPing :: Contact -> IO (Maybe ())
shouldPing c | isBad c = pure $ Just ()
shouldPing c = do 
    bef <- lastHeard c
    aft <- now
    if aft-bef > nodeTimeout
        then pure $ Just ()
        else return Nothing

pingFail :: Node -> Contact -> MaybeIO ()
pingFail nd c = MaybeT $ ping nd c >>= \pong -> case pong of 
    Nothing -> return $ pure ()
    otherwise -> return Nothing

update :: Info a => Node -> a -> MaybeIO ()
update nd@Node{router=tv} i = do
    r <- maybeIO $ readTVarIO tv
    let (up,r') = runState (R.update nd i) r
    case up of
        R.Replace (g1,b:g2) -> do
            maybeIO . putStrLn $ "try replace"
            let cs = contacts b
                end cs = maybeIO $ writeTVar tv $ reverse g1 ++ (withContacts b cs : g2)
                update' acc [] = end acc
                update' acc (hd:tl) = (do
                    MaybeT $ shouldPing hd
                    pingFail nd hd
                    if isBad hd
                        then end $ reverse tl ++ (fromInfo i : acc) 
                        else update' acc (bad hd : tl)) 
                    <|> update' (notBad hd : acc) tl 
            update' [] $ reverse cs
        otherwise -> maybeIO $ writeTVar tv r' 

fillTable :: Info a => Node -> [a] -> MaybeIO R.Result
fillTable nd q = iterFindNode nd q nd

generateTarget :: Bucket -> IdBytes -> MaybeIO IdBytes
generateTarget b rn = maybeIO $ do
    rg <- IdBytes.new $ range b
    let by1 = Id.bytes rn 
        by2 = Id.bytes rg
        rem = if Id.cmp rn rg == GT 
            then Binary.mod by1 by2 
            else Binary.mod by2 by1 
    IdBytes.new $ add (low b) rem

refreshBucket :: Node -> Bucket -> MaybeIO R.Result 
refreshBucket nd b = do
    rn <- maybeIO $ rand 
    tg <- generateTarget b rn
    iterFindNode nd (contacts b) tg 

iterFindNode :: (Info a,Id b) => Node -> [a] -> b -> MaybeIO R.Result
iterFindNode _ [] _ = maybeIO Nothing
iterFindNode nd q tg = 
    MaybeT (tryFindNode nd tg q') >>= find nd tg [] where q' = sortBy (Id.sort tg) q 

tryFindNode :: (Id a,Info b) => Node -> a -> [b] -> IO (Maybe (R.Result,b,[b]))
tryFindNode nd tg (hd:tl) = 
    findNode nd hd tg >>= \eit -> case eit of
        Right res -> return $ Just (res,hd,tl)
        otherwise -> tryFindNode nd tg tl
tryFindNode _ _ [] = return Nothing

find ::(Id a,Info b) => Node -> a -> [Contact] -> (R.Result,b,[b]) -> MaybeIO R.Result
find nd tg m (R.Contact c,i,_) | eq c tg = 
    MaybeT $ ping nd c >>= \res -> if null res
        then return . Just . R.Contacts $ fromInfo i : m
        else return . Just $ R.Contact c
find nd tg m (R.Contacts cs,i,q) =
    if null q'
        then maybeIO . Just $ R.Contacts m'
        else if Id.sort tg (head q') i == GT
            then maybeIO . Just $ R.Contacts m'
            else (MaybeT (tryFindNode nd tg q') >>= find nd tg m') <|> (maybeIO . Just $ R.Contacts m')
    where m' = fromInfo i : m
          q' = sortBy (Id.sort tg) $ cs ++ map fromInfo q
find nd tg m (_,i,q) =
    (MaybeT (tryFindNode nd tg q) >>= find nd tg m') <|> (maybeIO . Just $ R.Contacts m')
    where m' = fromInfo i : m
          