module BucketSpec (b1) where 

import Test.Hspec 
import Const (bucketSize)
import Control.Monad.State
import ContactFixt (defaultContacts)
import Binary (add,div2)
import Bucket
import IdBytes (new)

sz1 = bucketSize `div` 2
sz2 = sz1 `div` 2

g1 = take bucketSize defaultContacts
g2a = take sz1 g1 
g2b = drop sz1 g1
g3a = take sz2 g2a
g3b = drop sz2 g2a

b1 = withContacts defaultEmpty g1
b2 = withContacts defaultEmpty (last g1 : init g1) 
b3 = withContacts defaultEmpty (init $ contacts b2)

r1 = div2 defaultRangeBytes
r2 = div2 r1
r3 = div2 r2
r4 = div2 r3
r5 = div2 r4

Just l1 = IdBytes.new $ add defaultLowBytes r1
Just l2 = IdBytes.new $ add defaultLowBytes r2
Just l3 = IdBytes.new $ add defaultLowBytes r3
Just l4 = IdBytes.new $ add defaultLowBytes r4
Just l5 = IdBytes.new $ add defaultLowBytes r5

bi = Bucket.new g1 defaultLowId r1
bj = empty l1 r1
bk = Bucket.new g1 defaultLowId r2 
bl = empty l2 r2
bm = Bucket.new g2a defaultLowId r3
bn = Bucket.new g2b l3 r3
bq = Bucket.new g3a defaultLowId r4
br = Bucket.new g3b l4 r4
bs = Bucket.new [head g3a] defaultLowId r5
bt = Bucket.new [last g3a] l5 r5

pushContacts :: (Bool,Bucket) 
pushContacts = 
    foldr (\c (res,b) -> let (res',b') = runState (push c) b in (res && res',b')) (True,defaultEmpty) g1

tryPushContact :: (Bool,Bucket)
tryPushContact = runState (push $ defaultContacts !! bucketSize) b1

moveContact :: (Bool,Bucket)
moveContact = runState (move $ last g1) b1

tryMoveContact :: (Bool,Bucket)
tryMoveContact = runState (move $ defaultContacts !! bucketSize) b2

removeContact :: (Bool,Bucket)
removeContact = runState (remove . last $ contacts b2) b2

tryRemoveContact :: (Bool,Bucket)
tryRemoveContact = runState (remove . last $ contacts b2) b3

splitBucket :: Bucket -> Bool
splitBucket b =
    bx < by &&
    low bx == low b &&
    high by == high b &&
    high bx == low by && 
    range bx == range by &&
    and (map (\c -> if inRange bx c 
        then c `elem` csx && (not $ c `elem` csy)
        else c `elem` csy && (not $ c `elem` csx)) cs)
    where (by,bx) = split b
          cs = contacts b 
          csx = contacts bx 
          csy = contacts by

main :: IO ()
main = hspec $ do
    describe "Bucket" $ do
        it "pushes contacts" $ 
            pushContacts `shouldBe` (True,b1)
        it "tries to push another contact" $ 
            tryPushContact `shouldBe` (False,b1)
        it "moves contact to front" $ 
            moveContact `shouldBe` (True,b2)
        it "tries to move contact not in bucket" $ 
            tryMoveContact `shouldBe` (False,b2)
        it "removes contact" $ 
            removeContact `shouldBe` (True,b3)
        it "tries to remove contact again" $
            tryRemoveContact `shouldBe` (False,b3)
        it "checks first split" $ 
            split b1 `shouldBe` (bi,bj) 
        it "checks second split" $ 
            split bi `shouldBe` (bk,bl)
        it "checks third split" $ 
            split bk `shouldBe` (bm,bn)
        it "checks fourth split" $ 
            split bm `shouldBe` (bq,br)
        it "checks fifth split" $ 
            split bq `shouldBe` (bs,bt)

