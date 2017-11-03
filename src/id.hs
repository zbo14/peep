module Id
(
    Id,
    Id.bytes,
    Id.cmp,
    Id.sort,
    eq
) where

import Binary (cmp,sort)
import Data.ByteString.Lazy (ByteString)

class Id a where
    bytes :: a -> ByteString 

eq :: (Id a,Id b) => a -> b -> Bool
eq x y = Id.bytes x == Id.bytes y

cmp :: (Id a,Id b) => a -> b -> Ordering
cmp x y = Binary.cmp (Id.bytes x) (Id.bytes y)

sort :: (Id a,Id b,Id c) => a -> b -> c -> Ordering
sort x y z = Binary.sort (Id.bytes x) (Id.bytes y) (Id.bytes z)