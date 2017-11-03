module BitfieldSpec where 

import Test.Hspec
import Control.Monad.State (runState)
import Bitfield

l = 100
xs = [1,12,23,34,45,56,67,78,89,91]
ys = [32,54,87]
Just bf = Bitfield.new l xs
e = Bitfield.empty l

adds :: [Int] -> Bitfield -> IO Bitfield
adds [] bf = return bf
adds (x:xt) bf = do 
    let (res,bf') = runState (add x) bf
    res `shouldBe` True 
    bf `shouldNotBe` bf'
    adds xt bf'

addFail :: Int -> Bitfield -> IO ()
addFail x bf = do 
    let (res,bf') = runState (add x) bf
    res `shouldBe` False 
    bf `shouldBe` bf'
    return ()

hasMult :: [Int] -> Bitfield -> IO ()
hasMult [] _ = return ()
hasMult (x:xt) bf = do 
    has x bf `shouldBe` True 
    hasMult xt bf

doesntHave :: [Int] -> Bitfield -> IO ()
doesntHave [] _ = return ()
doesntHave (x:xt) bf = do 
    has x bf `shouldBe` False 
    doesntHave xt bf

main :: IO ()
main = hspec $ do 
    describe "Bitfield" $ do
        it "has multiple" $ hasMult xs bf
        it "adds multiple, checks that it has added values" $ do  
            bf' <- adds ys bf
            hasMult ys bf'
        it "add (fail)" $ addFail (-1) bf
        it "add (fail)" $ addFail 100 bf
        it "doesnt have" $ doesntHave [-2,101,99,10,11,36,68] bf
        it "adds to empty bitfield, check that it matches original" $ do 
            bf' <- adds xs e
            bf `shouldBe` bf'
            return ()


