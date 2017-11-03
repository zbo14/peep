{-# LANGUAGE MultiParamTypeClasses #-}

module File
(
    File,
    bitfield,
    pieces,
    name,
    size,
    File.new,
    File.empty
)
where 

import qualified Chunk as CH
import qualified Collection as C
import qualified Producer as P
import qualified Whole as W

import Bitfield (Bitfield,new)
import Piece (Piece)
import Data.ByteString.Lazy (ByteString,concat)
import Util (bLength)

data File = File {
    pieces :: [Piece],
    name :: String,
    size :: Int
} deriving (Show,Eq)

instance Ord File where 
    File{name=nm1} `compare` File{name=nm2} = nm1 `compare` nm2

instance CH.Chunk File where
    bytes File{pieces=pcs} = Data.ByteString.Lazy.concat $ map CH.bytes pcs
    size = size
    empty File{name=nm,size=sz} = 
        File {
            pieces=[],
            name=nm,
            size=sz
        }

instance C.Collection File Piece where
    chunks = pieces 
    withChunks pcs File{name=nm,size=sz} = File{pieces=pcs,name=nm,size=sz}

instance W.Whole File where 
    isComplete = and . map W.isComplete . pieces

new :: ByteString -> String -> File
new by nm = 
    File {
        pieces=pcs,
        name=nm,
        size=sz
    } where pcs = P.many by
            sz = bLength by

empty :: [Piece] -> String -> Int -> File
empty pcs nm sz = 
    File {
        pieces=pcs,
        name=nm,
        size=sz
    } 

bitfield :: File -> Maybe Bitfield
bitfield File{pieces=pcs} = Bitfield.new (length pcs) . map CH.index $ filter W.isComplete pcs