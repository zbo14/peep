module Binary
(
    add,
    cmp,
    diff,
    Binary.div,
    div2,
    Binary.mod,
    mul,
    mul2,
    sub,
    next,
    prev,
    btoi,
    itob,
    pad,
    padTo,
    sort
) where

import Data.Bits (shiftR,(.&.))
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8)

pad :: Int -> BS.ByteString -> BS.ByteString
pad p b = BS.replicate (fromIntegral p) 0 `mappend` b

padTo :: Int -> BS.ByteString -> BS.ByteString
padTo p b | p <= fromIntegral (BS.length b) = b
padTo p b = pad (p - fromIntegral (BS.length b)) b

add :: BS.ByteString -> BS.ByteString -> BS.ByteString
add = fn2 (+)

cmp :: BS.ByteString -> BS.ByteString -> Ordering 
cmp b1 b2 = btoi b1 `compare` btoi b2

sort :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Ordering
sort x y z = Binary.cmp (diff x y) (diff x z)

next :: BS.ByteString -> BS.ByteString
next = fn1 (+1)

prev :: BS.ByteString -> BS.ByteString
prev = fn1 (subtract 1)

mul2 :: BS.ByteString -> BS.ByteString
mul2 = fn1 (*2)

diff' :: Integer -> Integer -> Integer
diff' x y = toInteger (abs (x-y))

diff :: BS.ByteString -> BS.ByteString -> BS.ByteString
diff = fn2 diff'

div :: BS.ByteString -> BS.ByteString -> BS.ByteString
div = fn2 (Prelude.div)

mod :: BS.ByteString -> BS.ByteString -> BS.ByteString
mod = fn2 (Prelude.mod)

div2 :: BS.ByteString -> BS.ByteString
div2 = fn1 (`Prelude.div` 2)

mul :: BS.ByteString -> BS.ByteString -> BS.ByteString
mul = fn2 (*)

sub :: BS.ByteString -> BS.ByteString -> BS.ByteString
sub = fn2 (-)

fn1 :: (Integer -> Integer) -> BS.ByteString -> BS.ByteString
fn1 op = itob . op . btoi

fn2 :: (Integer -> Integer -> Integer) -> BS.ByteString -> BS.ByteString -> BS.ByteString
fn2 op b1 b2 = itob . op (btoi b1) . btoi $ b2

-- adapted from https://stackoverflow.com/a/39809366

btoi :: BS.ByteString -> Integer
btoi = wtoi 0 . BS.unpack 

itob :: Integer -> BS.ByteString
itob = BS.pack . itow []

itow :: [Word8] -> Integer -> [Word8]
itow ws i =
    let x = i `shiftR` 8
        w = fromIntegral (i .&. 0xFF) in
        if x == 0
            then w : ws
            else itow (w : ws) x

wtoi :: Integer -> [Word8] -> Integer
wtoi result [] = result 
wtoi result (w:ws) = wtoi (256 * result + (fromIntegral w)) ws
