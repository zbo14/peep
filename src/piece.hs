{-# LANGUAGE MultiParamTypeClasses #-}

module Piece 
(
    Piece,
    empty,
    blocks,
    hash,
    index,
    size
)
where 

import qualified Collection as C
import qualified Chunk as CH
import qualified Producer as P
import qualified Whole as W

import Block (Block)
import Const (pieceSize)
import Id (Id,bytes)
import Data.ByteString.Lazy (ByteString,concat)
import Util (bLength,sha1)

data Piece = Piece {
    blocks :: [Block],
    hash :: ByteString,
    index :: Int,
    size :: Int
} deriving (Show,Eq)

empty :: Id a => a -> Int -> Int -> Piece
empty h idx sz = 
    Piece {
        blocks=[],
        hash=bytes h,
        index=idx,
        size=sz
    }

instance Ord Piece where 
    Piece{index=idx1} `compare` Piece{index=idx2} = idx1 `compare` idx2

instance CH.Chunk Piece where
    bytes = Data.ByteString.Lazy.concat . map CH.bytes . blocks
    size = size 
    index = index
    defaultSize _ = pieceSize
    empty Piece{hash=h,index=idx,size=sz} = 
        Piece{
            blocks=[],
            hash=h,
            index=idx,
            size=sz
        }

instance P.Producer Piece where 
    one by idx = Piece {blocks=bls,index=idx,hash=h,size=sz}
        where bls = P.many by :: [Block]
              sz = bLength by
              h = sha1 by

instance C.Collection Piece Block where
    chunks = blocks
    withChunks bls Piece{hash=h,index=idx,size=sz} = Piece{blocks=bls,hash=h,index=idx,size=sz}

instance W.Whole Piece where
    isComplete pc@Piece{hash=h} = h == sha1 (CH.bytes pc)
