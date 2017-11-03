module BinarySpec where 

import qualified Binary as B
import Test.Hspec

i1 = 123456789
i2 = 9012345678

b1 = B.itob i1 
b2 = B.itob i2

added = B.btoi $ B.add b1 b2
subtracted = B.btoi $ B.sub b2 b1
divided = B.btoi $ B.div b2 b1
multiplied = B.btoi $ B.mul b1 b2
modulo = B.btoi $ B.mod b2 b1
difference = B.btoi $ B.diff b1 b2

main :: IO ()
main = hspec $ do
	describe "Binary" $ do 
		it "checks added bytes" $
			added `shouldBe` i1+i2
		it "checks subtracted bytes" $ 
			subtracted `shouldBe` i2-i1
		it "checks divided bytes" $ 
			divided `shouldBe` i2`div`i1
		it "checks multiplied bytes" $ 
			multiplied `shouldBe` i1*i2
		it "checks modulo bytes" $ 
			modulo `shouldBe` i2`mod`i1
		it "checks difference bytes" $
			difference `shouldBe` subtracted