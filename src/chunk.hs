module Chunk 
(
    Chunk,
    bytes,
    empty,
    size,
    index,
    defaultSize
) where 

import Data.ByteString.Lazy (ByteString)

class (Ord a,Show a) => Chunk a where 
    bytes :: a -> ByteString
    empty :: a -> a
    size :: a -> Int
    index :: a -> Int
    index _ = 0
    defaultSize :: a -> Int
    defaultSize = size