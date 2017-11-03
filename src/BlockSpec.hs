module BlockSpec (blocks) where

import Test.Hspec
import MediaFixt (media)
import ProducerFixt
import Block
import Producer (many)

blocks = media >>= \by -> return $ many by :: IO [Block]

main :: IO ()
main = do
    by <- media
    bls@(hd:tl) <- blocks
    hspec $ do
        describe "Block" $ do
            it "test producer" $ testProducer bls by