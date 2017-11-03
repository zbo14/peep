module FileSpec where 

import Test.Hspec 
import PieceSpec (pieces)
import MediaFixt (media)
import CollectionFixt

import Chunk (empty)
import Piece (Piece)
import File (File,new)
import Control.Monad.State
import Collection (Update,lookup,update,)

update' = update :: Piece -> State File Update
lookup' = Collection.lookup :: Int -> File -> Maybe Piece 

main :: IO ()
main = do
    by <- media
    let f = new by "test.mp3"
        e = empty f  
    pcs@(p:_) <- pieces
    hspec $ do
        describe "File" $ do
            it "test insert" $ do 
                testInsert update' p e
                return ()
            it "test replace" $ do 
                testReplace update' (empty p) f
                return ()
            it "test lookup" $ testLookup lookup' (Just p) 0 f
            it "test lookup (fail)" $ testLookup lookup' Nothing 0 e
            it "test fill" $ testFill update' pcs e