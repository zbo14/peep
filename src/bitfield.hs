module Bitfield
(
    Bitfield,
    add,
    has,
    empty,
    new,
    withBytes,
    bytes
) 
where 

import qualified Whole as W

import Control.Monad.State (State,state)
import Data.Word (Word8)
import Data.Bits (shiftL,(.|.),(.&.))
import qualified Data.ByteString.Lazy as BS
import Util (bIndex,bLength,bSplitAt,bReplicate)

data Bitfield = Bitfield {
  bytes :: BS.ByteString,
  complete :: BS.ByteString,
  size :: Int
} deriving (Show,Eq)

instance W.Whole Bitfield where 
    isComplete Bitfield{bytes=by,complete=c} = by == c

add :: Int -> State Bitfield Bool 
add = state . add'

add' :: Int -> Bitfield -> (Bool,Bitfield)
add' x bf@Bitfield{size=sz} | x < 0 || x >= sz = (False,bf)
add' x bf@Bitfield{bytes=b,complete=c,size=sz} = 
     case BS.uncons b2 of 
        Just (h,tl) -> let y = 1 `shiftL` fromIntegral md in if h .&. y /= 0 then (False,bf) 
            else (True,Bitfield {
                bytes=b1 `mappend` BS.cons (h .|. y) tl,
                complete=c,
                size=sz
            })
        Nothing -> (False,bf)
    where md = x `mod` 8 
          idx = x `div` 8 
          (b1,b2) = bSplitAt idx b

has :: Int -> Bitfield -> Bool 
has x Bitfield{size=sz} | x < 0 || x >= sz = False 
has x Bitfield{bytes=by} = (bIndex (fromIntegral idx) by) .&. (1 `shiftL` fromIntegral md) /= 0
    where idx = x `div` 8
          md = x `mod` 8

withBytes :: Int -> BS.ByteString -> Maybe Bitfield
withBytes sz by = 
    if bLength by /= bLength by' then Nothing else pure Bitfield{bytes=by,complete=c,size=sz}
    where Bitfield{bytes=by',complete=c} = empty sz

empty :: Int -> Bitfield 
empty sz = 
    Bitfield{bytes=by',complete=c,size=sz}
    where md = sz `mod` 8
          dv = sz `div` 8
          by = bReplicate dv 255 
          c = if md == 0 then by else BS.snoc by $ foldr (\n res -> res .|. ((1 `shiftL` n) :: Word8)) (0 :: Word8) [0..md-1]
          by' = bReplicate (bLength c) 0

new :: Int -> [Int] -> Maybe Bitfield 
new sz = fn (Bitfield.empty sz)
    where fn bf [] = pure bf
          fn bf (x:xt) = if res 
            then fn bf' xt 
            else Nothing 
            where (res,bf') = add' x bf