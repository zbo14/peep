{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Util (
    MaybeIO,
    ExceptIO,
    ExceptSTM,
    maybeIO,
    exceptIO,
    exceptSTM,
    toExceptIO,
    portNum,
    now, 
    enumerate,
    bIndex,
    bLength,
    bSplitAt,
    bTake,
    bDrop,
    bReplicate,
    chunkBytes,
    splice, 
    inserts, 
    insert, 
    replaces, 
    replace,
    remove,
    sha1,
    encodeWord16,
    decodeWord16,
    encodeWord32,
    decodeWord32,
    encodeWord,
    decodeWord
) where 

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Concurrent.STM
import Data.Bits (shiftR,shiftL,(.&.))
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8,Word16,Word32,Word)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket (PortNumber)
import Crypto.Hash.SHA1 (hashlazy)

type MaybeIO = MaybeT IO

class MaybeIO' f where 
  maybeIO :: f a -> MaybeIO a

toExceptIO :: MaybeIO' f => String -> f a -> ExceptIO a
toExceptIO s x = 
  ExceptT $ runMaybeT (maybeIO x) >>= \y -> case y of 
    Just z -> return $ Right z
    otherwise -> return $ Left s

instance MaybeIO' MaybeIO where 
  maybeIO = id

instance MaybeIO' STM where 
  maybeIO = maybeIO . atomically

instance MaybeIO' IO where 
  maybeIO x = MaybeT $ x >>= return . Just

instance MaybeIO' Maybe where 
  maybeIO = MaybeT . return

type ExceptIO = ExceptT String IO

class ExceptIO' f where 
  exceptIO :: f a -> ExceptIO a

instance ExceptIO' IO where 
  exceptIO x = ExceptT $ x >>= return . Right

instance ExceptIO' STM where 
  exceptIO = exceptIO . atomically

instance ExceptIO' (Either String) where 
  exceptIO = ExceptT . return

instance ExceptIO' ExceptSTM where 
  exceptIO = ExceptT . atomically . runExceptT

-- instance ExceptIO' ExceptIO where 
--   exceptIO = id

type ExceptSTM = ExceptT String STM 

class ExceptSTM' f where 
  exceptSTM :: f a -> ExceptSTM a 

instance ExceptSTM' STM where 
  exceptSTM x = ExceptT $ x >>= return . Right

instance ExceptSTM' (Either String) where
  exceptSTM = ExceptT . return

enumerate :: [a] -> [(Int,a)]
enumerate = zip [0..]

bIndex ::  Integral a => a -> BS.ByteString -> Word8
bIndex x b = BS.index b $ fromIntegral x

bLength :: BS.ByteString -> Int
bLength = fromIntegral . BS.length

bSplitAt ::  Integral a => a -> BS.ByteString -> (BS.ByteString,BS.ByteString) 
bSplitAt x b = BS.splitAt (fromIntegral x) b

bTake :: Integral a => a -> BS.ByteString -> BS.ByteString
bTake x b = BS.take (fromIntegral x) b

bDrop ::  Integral a => a -> BS.ByteString -> BS.ByteString 
bDrop x b = BS.drop (fromIntegral x) b

bReplicate ::  Integral a => a -> Word8 -> BS.ByteString 
bReplicate n w = BS.replicate (fromIntegral n) w 

portNum :: Word16 -> PortNumber
portNum w = (fromIntegral w) :: PortNumber

now :: IO Int
now = round <$> getPOSIXTime

splice :: Int -> Int -> [a] -> [a] -> [a]
splice idx del xs lst = (take idx lst) ++ xs ++ (drop (idx+del) lst)

inserts :: Int -> [a] -> [a] -> [a]
inserts idx xs = splice idx 0 xs

insert :: Int -> a -> [a] -> [a]
insert idx x = inserts idx [x]

replaces :: Int -> [a] -> [a] -> [a]
replaces idx xs = splice idx 1 xs

replace :: Int -> a -> [a] -> [a]
replace idx x = replaces idx [x]

remove :: Int  -> Int -> [a] -> [a]
remove idx del = splice idx del []

sha1 :: BS.ByteString -> BS.ByteString
sha1 = BS.fromStrict . hashlazy

chunkBytes :: Int -> BS.ByteString -> [BS.ByteString]
chunkBytes = chunkBytes' []

chunkBytes' :: [BS.ByteString] -> Int -> BS.ByteString -> [BS.ByteString]
chunkBytes' acc _ b | BS.null b = reverse acc
chunkBytes' acc n b = chunkBytes' (h:acc) n tl where (h,tl) = BS.splitAt (fromIntegral n) b 

decodeWord :: [Word8] -> Word 
decodeWord ws = 
    foldr (\(i,w) acc -> (+acc) . fromIntegral $ w `shiftL` (i*8)) 0 (zip [0..] ws)

encodeWord :: Int -> Word -> [Word8]
encodeWord = encodeWord' [] 0

encodeWord' :: [Word8] -> Int -> Int -> Word -> [Word8]
encodeWord' acc n l _ | l == n = acc
encodeWord' acc n l w = 
    encodeWord' (w8:acc) (n+1) l w
    where x = n*8
          w8 = fromIntegral $ (w .&. (255 `shiftL` x)) `shiftR` x

encodeWord16 :: Word16 -> [Word8]
encodeWord16 w16 = 
    map fromIntegral [ (w16 .&. 0xFF00) `shiftR` 8, w16 .&. 0xFF ]

decodeWord16 :: (Word8,Word8) -> Word16 
decodeWord16 (w1,w2) = x1 + x2
    where x1 = (`shiftL`8) $ fromIntegral w1
          x2 = fromIntegral w2

encodeWord32 :: Word32 -> [Word8]
encodeWord32 w32 = 
    map fromIntegral [ (w32 .&. 0xFF000000) `shiftR` 24, (w32 .&. 0xFF0000) `shiftR` 16 ,(w32 .&. 0xFF00) `shiftR` 8, w32 .&. 0xFF ]

decodeWord32 :: (Word8,Word8,Word8,Word8) -> Word32 
decodeWord32 (w1,w2,w3,w4) = x1 + x2 + x3 + x4
    where x1 = (`shiftL`24) $ fromIntegral w1
          x2 = (`shiftL`16) $ fromIntegral w2
          x3 = (`shiftL` 8) $ fromIntegral w3
          x4 = fromIntegral w4