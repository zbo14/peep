module BencodeSpec where

import qualified Data.ByteString.Lazy.Char8 as C8

import Bencode (encode,decode)
import Value (Value(I,B,L,D),newDict,wrap)
import Test.Hspec

byteString = C8.pack "bencode"
wrappedByteString = wrap byteString
encodedByteString = encode wrappedByteString
decodedByteString= decode encodedByteString

int = 1001 :: Int
wrappedInt = wrap int
encodedInt = encode wrappedInt
decodedInt = decode encodedInt

list = [wrappedByteString,wrappedInt]
wrappedList = wrap list
encodedList = encode wrappedList
decodedList = decode encodedList

dict = newDict [("byteString",wrappedByteString),("int",wrappedInt),("list", wrappedList)]
wrappedDict = wrap dict
encodedDict = encode wrappedDict
decodedDict = decode encodedDict

main :: IO ()
main = hspec $ do 
    describe "Bencode" $ do 
        it "checks encoded string" $
            encodedByteString `shouldBe` C8.pack "7:bencode"
        it "checks decoded string" $
            decodedByteString `shouldBe` Just (B byteString,9,C8.empty)
        it "checks encoded int" $ 
            encodedInt `shouldBe` C8.pack "i1001e"
        it "checks decoded int" $
            decodedInt `shouldBe` Just (I int,6,C8.empty)
        it "checks encoded list" $
            encodedList `shouldBe` C8.pack "l7:bencodei1001ee"
        it "checks decoded list" $
            decodedList `shouldBe` Just (L list,17,C8.empty)
        it "checks encoded dict" $
            encodedDict `shouldBe` C8.pack "d10:byteString7:bencode3:inti1001e4:listl7:bencodei1001eee"
        it "checks decoded dict" $
            decodedDict `shouldBe` Just (D dict,58,C8.empty)
