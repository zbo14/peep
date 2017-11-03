module RoutingSpec where 

import Test.Hspec 
import Const (bucketSize)
import Control.Monad.State
import Bucket (Bucket,withContacts,split)
import BucketSpec (b1)
import Contact (Contact)
import ContactFixt (defaultContacts)
import qualified Routing as RT

numGroups = length defaultContacts `div` bucketSize 
gs = foldr (\n acc -> (take bucketSize $ drop (n*bucketSize) defaultContacts) : acc) [] [0..numGroups-1]

(bi,bj) = let (bi,bj) = split b1 in (bi, withContacts bj (last gs))
(bk,bl) = let (bk,bl) = split bi in (bk, withContacts bl (head $ tail gs))

me = head defaultContacts
r = RT.new
r1 = [b1]
r2 = [bi,bj]
r3 = [bk,bl,bj]

addContacts :: [Bucket] -> [Contact] -> (RT.Update, [Bucket])
addContacts r cs = 
    foldr (\c (up,r) -> case up of
        RT.Push -> runState (RT.update me c) r
        otherwise -> (RT.None,r)) (RT.Push,r) cs

main :: IO ()
main = hspec $ do
    describe "Routing" $ do 
        it "adds contacts to routing table" $ do 
            addContacts r (head gs) `shouldBe` (RT.Push, r1)
        it "adds more contacts to routing table (bucket split)" $ do
            addContacts r1 (last gs) `shouldBe` (RT.Push, r2)
        it "adds more contacts to routing table (bucket split)" $ do 
            addContacts r2 (head $ tail gs) `shouldBe` (RT.Push, r3)
        it "tries to add more contacts (no bucket split)" $ do 
            addContacts r3 (last $ init gs) `shouldBe` (RT.None, r3)
        it "finds contact" $ do
            RT.find (last $ head gs) r3 `shouldBe` Just (RT.Contact . last $ head gs)
        it "finds closest" $ do 
            RT.find (last . last $ init gs) r3 `shouldBe` Just (RT.Contacts $ last gs)