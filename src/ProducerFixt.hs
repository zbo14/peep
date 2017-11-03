module ProducerFixt (testProducer) where

import Test.Hspec (shouldBe)
import Chunk (size,index,defaultSize)
import Producer (Producer,one)
import Data.ByteString.Lazy (ByteString,null)
import Data.List (sortBy)
import Util (bSplitAt)

testProducer :: Producer a => [a] -> ByteString -> IO ()
testProducer xs@(hd:_) b = do 
    let cmp p1 p2 = index p1 `compare` index p2
    sortBy compare xs `shouldBe` sortBy cmp xs
    testProducer' 0 xs b

testProducer' :: Producer a => Int -> [a] -> ByteString -> IO ()
testProducer' idx (hd:[]) b = do
    let dsz = defaultSize hd
        (b1,b2) = bSplitAt dsz b
        x = one b1 idx
    x `shouldBe` hd
    Data.ByteString.Lazy.null b2 `shouldBe` True
    return ()
testProducer' idx (hd:tl) b = do
    let dsz = defaultSize hd
        (b1,b2) = bSplitAt dsz b
        x = one b1 idx
    x `shouldBe` hd
    size x `shouldBe` dsz
    testProducer' (idx+1) tl b2