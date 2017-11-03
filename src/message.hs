module Message
(
    Tuple,
    query,
    response,
    queryPing,
    queryFindNode,
    queryGetPeers,
    messageToTuple
) where

import Id (Id,bytes)
import IdBytes (IdBytes,new)
import Data.Word (Word16)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Value(Dict,newDict,emptyDict,dictInsert,dictLookup,wrap,unwrap)

type Tuple = (String,String,ByteString,IdBytes,Dict)

new :: ByteString -> ByteString -> Dict
new y t = 
    dictInsert "y" y . dictInsert "t" t $ emptyDict

query :: Id a => ByteString -> Dict -> a -> ByteString -> Dict
query q a k =
    dictInsert "id" (bytes k) . dictInsert "a" a . dictInsert "q" q . Message.new (pack "q")

response :: Id a => Dict -> a -> ByteString-> Dict
response r k = 
    dictInsert "id" (bytes k) . dictInsert "r" r . Message.new (pack "r")

queryPing :: Id a => a -> ByteString -> Dict
queryPing = query (pack "ping") emptyDict

queryFindNode :: (Id a,Id b) => a -> b -> ByteString -> Dict
queryFindNode tg = 
    query (pack "find_node") (dictInsert "target" (bytes tg) emptyDict)

queryGetPeers :: (Id a,Id b) => a -> b -> ByteString -> Dict
queryGetPeers h =
    query (pack "get_peers") (dictInsert "info_hash" (bytes h) emptyDict) 

queryAnnouncePeer :: (Id a,Id b) => a -> Word16 -> ByteString -> b -> ByteString -> Dict
queryAnnouncePeer h p tk = 
    query (pack "announce_peer") . newDict $ [("info_hash",wrap $ bytes h),("port",wrap ((fromIntegral p) :: Int)),("token",wrap tk)]

queries = [pack "ping",pack "find_node"]

messageToTuple :: Dict -> Maybe Tuple 
messageToTuple m = do
    y <- unwrap =<< dictLookup "y" m :: Maybe ByteString
    t <- unwrap =<< dictLookup "t" m :: Maybe ByteString
    i <- unwrap =<< dictLookup "id" m :: Maybe ByteString
    case unpack y of
        "q" -> do 
            i' <- IdBytes.new i
            q <- (\q -> if elem q queries then return q else Nothing) =<< (unwrap =<< dictLookup "q" m :: Maybe ByteString)
            a <- dictLookup "a" m >>= unwrap :: Maybe Dict
            return (unpack y,unpack q,t,i',a)
        "r" ->  do 
            i' <- IdBytes.new i
            r <- dictLookup "r" m >>= unwrap :: Maybe Dict
            return (unpack y,[],t,i',r)
        otherwise -> Nothing 


