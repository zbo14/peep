{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CollectionFixt 
(
    testInsert,
    testReplace,
    testNone,
    testLookup,
    testFill
)
where

import Test.Hspec (shouldBe,shouldNotBe)
import Control.Monad.State
import Collection
import Whole (isComplete)

testFill :: Collection a b => (b -> State a Update) -> [b] -> a -> IO ()
testFill _ [] c = do 
    isComplete c `shouldBe` True 
    return ()
testFill update (hd:tl) c = do
    c' <- testInsert update hd c
    testFill update tl c'

isInsert :: Update -> Bool 
isInsert (Insert _) = True 
isInsert _ = False

testInsert :: Collection a b => (b -> State a Update) -> b -> a -> IO a
testInsert update ch c = do
    let (res,c') = runState (update ch) c
    isInsert res `shouldBe` True
    c `shouldNotBe` c'
    return c'

isReplace :: Update -> Bool
isReplace (Replace _) = True
isReplace _ = False

testReplace :: Collection a b => (b -> State a Update) -> b -> a -> IO a
testReplace update ch c = do 
    let (res,c') = runState (update ch) c
    isReplace res `shouldBe` True
    c `shouldNotBe` c'
    return c'

isNone :: Update -> Bool 
isNone None = True 
isNone _ = False

testNone :: Collection a b => (b -> State a Update) -> b -> a -> IO a
testNone update ch c = do 
    let (res,c') = runState (update ch) c
    isNone res `shouldBe` True 
    c `shouldBe` c'
    return c

testLookup :: Collection a b => (Int -> a -> Maybe b) -> Maybe b -> Int -> a -> IO ()
testLookup lookup exp idx c = do
    exp `shouldBe` lookup idx c
    return ()