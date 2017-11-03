module Bencode
(
    encode,
    decode
) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Map (empty,insert,toList)
import Util (bDrop,bLength,bTake)
import Value (Dict,List,Value(I,B,D,L))

encode :: Value -> BS.ByteString
encode (I i) = encodeInt i
encode (B b) = encodeByteString b
encode (D d) = encodeDict d
encode (L l) = encodeList l

decode :: BS.ByteString -> Maybe (Value,Int,BS.ByteString)
decode b = case C8.uncons b of 
    Just ('i',bt) -> decodeInt bt >>= \(res,i,b') -> return (I res,i,b')
    Just ('l',bt) -> decodeList bt >>= \(res,i,b') -> return (L res,i,b')
    Just ('d',bt) -> decodeDict bt >>= \(res,i,b') -> return (D res,i,b')
    Just _ -> decodeByteString b >>= \(res,i,b') -> return (B res,i,b')
    otherwise -> Nothing

segment :: Char -> BS.ByteString -> Maybe (BS.ByteString,BS.ByteString)
segment = segment' BS.empty

segment' :: BS.ByteString -> Char -> BS.ByteString -> Maybe (BS.ByteString,BS.ByteString)
segment' res c b = case C8.uncons b of 
    Just (h,bt) -> if c == h 
        then return (BS.reverse res,bt)
        else segment' (C8.cons h res) c bt
    otherwise -> Nothing

decodeInt :: BS.ByteString -> Maybe (Int,Int,BS.ByteString)
decodeInt b = segment 'e' b >>= decodeInt'

decodeInt' :: (BS.ByteString,BS.ByteString) -> Maybe (Int,Int,BS.ByteString)
decodeInt' (b1,b2) = return (x,n,b2) 
    where x = fromIntegral . read $ C8.unpack b1
          n = bLength b1 + 2

decodeByteString :: BS.ByteString -> Maybe (BS.ByteString,Int,BS.ByteString)
decodeByteString b = segment ':' b >>= decodeByteString'

decodeByteString' :: (BS.ByteString,BS.ByteString) -> Maybe (BS.ByteString,Int,BS.ByteString)
decodeByteString' (b1,b2) = 
    if bLength b' /= r 
        then Nothing 
        else return (b',r',b'')
    where r = fromIntegral . read $ C8.unpack b1
          r' = bLength b1 + 1 + r
          b' = bTake r b2 
          b'' = bDrop r b2 

decodeList :: BS.ByteString -> Maybe (List,Int,BS.ByteString)
decodeList = decodeList' [] 2

decodeList' :: List -> Int -> BS.ByteString -> Maybe (List,Int,BS.ByteString)
decodeList' l r b = case C8.uncons b of
    Just ('e',bt) -> return (reverse l,r,bt)
    Just _ -> decode b >>= \(v,r',b') -> decodeList' (v:l) (r+r') b'
    otherwise -> Nothing

decodeDict :: BS.ByteString -> Maybe(Dict,Int,BS.ByteString)
decodeDict = decodeDict' empty 2

decodeDict' :: Dict -> Int -> BS.ByteString -> Maybe(Dict,Int,BS.ByteString)
decodeDict' d r b = case C8.uncons b of 
    Just ('e',bt) -> return (d,r,bt)
    Just _ -> do
        (k,r',b') <- decodeByteString b
        (v,r'',b'') <- decode b'
        decodeDict' (insert k v d) (r+r'+r'') b''
    otherwise -> Nothing

encodeInt :: Int -> BS.ByteString
encodeInt i = C8.pack $ 'i' : show i ++ "e"

encodeByteString :: BS.ByteString -> BS.ByteString
encodeByteString b = (C8.pack . show $ bLength b) `mappend` C8.cons ':' b

encodeList :: [Value] -> BS.ByteString
encodeList l = 
    C8.snoc (C8.cons 'l' $ BS.concat $ map encode l) 'e'

encodeDict :: Dict -> BS.ByteString
encodeDict d = 
    C8.snoc (C8.cons 'd' $ BS.concat . map (\(k,v) -> encodeByteString k `mappend` encode v) $ toList d) 'e'