{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BigEndian 
(
    Encoder,
    Decoder,
    encodeBytes,
    encodeBuilder,
    decodeBytes,
    decodeBuilder
) where

import Data.Bits (shiftR)
import Data.ByteString.Builder (Builder,word32BE,lazyByteString,toLazyByteString)
import Data.ByteString.Lazy (ByteString,pack,unpack)
import Data.Word (Word32)
import Util (bLength,bSplitAt,encodeWord,encodeWord32,encodeWord,decodeWord,decodeWord32)

class (Ord a,Show a) => Encoder a where 
    encodeBytes :: a -> ByteString
    encodeBuilder :: a -> Builder

instance Encoder Word32 where 
    encodeBuilder w = word32BE w
    encodeBytes = pack . encodeWord32

instance Encoder Int where 
    encodeBuilder = word32BE . fromIntegral
    encodeBytes = pack . encodeWord32 . fromIntegral

instance Encoder ByteString where 
    encodeBuilder b = word32BE l `mappend` lazyByteString b where l = fromIntegral $ bLength b
    encodeBytes b = pack (encodeWord32 l) `mappend` b where l = fromIntegral $ bLength b

instance Encoder Word where 
    encodeBuilder x = b1 `mappend` b2 
        where l = x `shiftR` 8 + 1
              b1 = word32BE $ fromIntegral l
              b2 = lazyByteString . pack $ encodeWord (fromIntegral l) x
    encodeBytes x = b1 `mappend` b2
        where l = x `shiftR` 8 + 1
              b1 = encodeBytes (fromIntegral l :: Word32)
              b2 = pack $ encodeWord (fromIntegral l) x

class (Ord a,Show a) => Decoder a where
    decodeBytes :: ByteString -> Maybe (a,ByteString)
    decodeBuilder :: Builder -> Maybe (a,Builder)
    decodeBuilder bd = decodeBytes b >>= \(res,b) -> let bd = lazyByteString b in pure (res,bd) where b = toLazyByteString bd

instance Decoder Word32 where 
    decodeBytes b =
        if bLength b < 4 then Nothing 
            else let (b1,b2) = bSplitAt 4 b
                     (w1:w2:w3:w4:_) = unpack b1
                     w = decodeWord32 (w1,w2,w3,w4) in pure (w,b2)

instance Decoder Int where 
    decodeBytes b = (decodeBytes b :: Maybe (Word32,ByteString)) >>= 
        \(w32,b) -> pure (fromIntegral w32,b)

instance Decoder ByteString where 
    decodeBytes b = fn1 b >>= fn2
        where fn1 b | bLength b < 4 = Nothing
              fn1 b = pure (b2,l)
                where (b1,b2) = bSplitAt 4 b
                      (w1:w2:w3:w4:_) = unpack b1
                      l = fromIntegral $ decodeWord32 (w1,w2,w3,w4) 
              fn2 (b,l) | bLength b < l = Nothing
              fn2 (b,l) = pure $ bSplitAt l b

instance Decoder Word where
    decodeBytes b = decodeBytes b >>= 
        \(b1,b2) -> let w = decodeWord $ unpack b1 in return (w,b2)