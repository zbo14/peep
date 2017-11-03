module ContactSpec where 

import Test.Hspec
import Network.Socket (hostAddressToTuple)
import Data.ByteString.Lazy (pack)
import Binary (itob,next,prev)
import Contact
import Id (bytes,sort)
import IdBytes (new)
import Info (bytes)
import InfoBytes (new)
import Util (sha1)

by1 = sha1 $ pack [1,2,3]
by2 = next by1
by3 = prev by1

Just id1 = IdBytes.new by1 
Just id2 = IdBytes.new by2
Just id3 = IdBytes.new by3

c1 = Contact.new id1 8888 (127,0,0,1)
c2 = Contact.new id2 8889 (127,0,0,1)
c3 = Contact.new id3 9000 (127,0,0,1)

info = Info.bytes c1
p = fromIntegral (Contact.port c1)
h = hostAddressToTuple $ Contact.host c1
Just c = InfoBytes.new (Info.bytes c1) >>= return . Contact.fromInfo

main :: IO ()
main = hspec $ do
    describe "Contact" $ do
        it "checks contact id" $ 
            Id.bytes c1 `shouldBe` by1
        it "checks port" $ 
            p `shouldBe` 8888
        it "checks host" $ 
            h `shouldBe` (127,0,0,1)
        it "checks contact info" $ 
            info `shouldBe` by1 `mappend` pack [127,0,0,1,34,184]
        it "checks contact equality" $
            c `shouldBe` c1
        it "checks that addresses match" $ 
            Contact.addr c1 `shouldBe` Contact.addr c
        it "checks contact comparison" $ 
            c1 `compare` c2 `shouldBe` LT
        it "checks contact sorting" $ 
            sort c2 c3 c1 `shouldBe` GT
