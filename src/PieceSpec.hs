module PieceSpec where

import Test.Hspec
import BlockSpec (blocks)
import MediaFixt (media)
import ProducerFixt 
import CollectionFixt

import Block
import Const (blockSize,pieceSize)
import Chunk (empty)
import qualified Piece as P
import Control.Monad.State (State)
import Producer (many)
import Collection

update' = update :: Block -> State P.Piece Update
lookup' = Collection.lookup :: Int -> P.Piece -> Maybe Block
pieces = media >>= \by -> return $ many by :: IO [P.Piece]

main :: IO ()
main = do
    by <- media
    pcs@(p:_) <- pieces
    let e = empty p
        nb = pieceSize `div` blockSize
    bls@(b:_) <- BlockSpec.blocks 
    hspec $ do
        describe "Piece" $ do
            it "test producer" $ testProducer pcs by
            it "test insert" $ do 
                testInsert update' b e
                return ()
            it "test replace" $ do 
                testReplace update' (empty b) p
                return ()
            it "test lookup" $ testLookup lookup' (Just b) 0 p
            it "test lookup (fail)" $ testLookup lookup' Nothing 5 e
            it "test fill" $ testFill update' (take nb bls) e