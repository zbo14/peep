module Block
(
    Block,
    bytes,
    index,
    size
)
where 

import Const (blockSize)
import Data.ByteString.Lazy (ByteString,empty)
import qualified Chunk as CH
import qualified Producer as P
import Util (bLength,bTake)

data Block = Block {
    bytes :: ByteString,
    index :: Int,
    size :: Int
} deriving (Eq)

instance Show Block where
    show Block{bytes=by,index=idx,size=sz} = "BLOCK {BYTES=" ++ show (bTake 4 by) ++ ".., INDEX=" ++ show idx ++ ", SIZE=" ++ show sz ++ "}"

instance Ord Block where 
    Block{index=idx1} `compare` Block{index=idx2} = idx1 `compare` idx2

instance CH.Chunk Block where 
    bytes = bytes
    size = size
    index = index
    defaultSize _ = blockSize
    empty Block{index=idx,size=sz} = Block{bytes=empty,index=idx,size=sz} 

instance P.Producer Block where
    one by idx = Block {bytes=by,index=idx,size=bLength by} 
