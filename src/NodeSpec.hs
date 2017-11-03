module NodeSpec where 

import Test.Hspec
import Const (bucketSize)
import ContactFixt (defaultContacts)
import Contact (fromInfo)
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import Node
import qualified Routing as R
import Util (remove)

take' = take bucketSize
nodes = map new defaultContacts

groups = groups' []

groups' :: [[Node]] -> [Node] -> [[Node]]
groups' acc [] = acc
groups' acc nds = groups' (g:acc) rst where (rst,g) = splitAt (length nds - bucketSize) nds

ping' :: Maybe () -> Node -> Node -> IO ()
ping' exp from to = ping from to >>= (`shouldBe` exp) >>= \_ -> return ()

findContacts :: Node -> Node -> [Node] -> IO ()
findContacts _ _ [] = pure ()
findContacts from to (hd:tl) = findContact from to hd >>= \_ -> findContacts from to tl

findContact :: Node -> Node -> Node -> IO ()
findContact from to tg = findNode from to tg >>= (`shouldBe` (Right . R.Contact $ contact tg)) >>= \_ -> return ()

findClosest :: [Node] -> Node -> Node -> Node -> IO ()
findClosest tgs = findNode' $ Right . R.Contacts $ map fromInfo tgs

findNode' :: Either String R.Result -> Node -> Node -> Node -> IO ()
findNode' exp from to tg = findNode from to tg >>= (`shouldBe` exp) >>= \_ -> return ()

iterFindNode' :: Maybe R.Result -> Node -> [Node] -> Node -> IO ()
iterFindNode' exp from q tg = runMaybeT (iterFindNode from q tg) >>= (`shouldBe` exp) >>= \_ -> return ()

iterFindContact :: Node -> [Node] -> Node -> IO ()
iterFindContact from q tg = iterFindNode' (Just . R.Contact $ contact tg) from q tg

iterFindClosest :: [Node] -> Node -> [Node] -> Node -> IO ()
iterFindClosest nds = iterFindNode' (Just . R.Contacts $ map fromInfo nds)

-- iterFindClosest

pingSuccess = ping' $ Just ()
pingFailure = ping' $ Nothing

pings :: Node -> [Node] -> IO ()
pings from [] = pure ()
pings from (hd:tl) = pingSuccess from hd >>= \_ -> pings from tl

pingSequence :: [Node] -> IO ()
pingSequence (_:[]) = pure ()
pingSequence (hd:tl) = pingSuccess hd (head tl) >>= \_ -> pingSequence tl

checkNumberBuckets :: Node -> Int -> IO ()
checkNumberBuckets nd exp = readTVarIO (router nd) >>= (`shouldBe` exp) . length >>= \_ -> return () 

checkContacts :: Node -> [Node] -> IO ()
checkContacts nd [] = pure ()
checkContacts nd (hd:tl) = do 
    checkContact nd hd
    checkContact hd nd 
    checkContacts nd tl

checkContact :: Node -> Node -> IO ()
checkContact nd info = checkRoutingTable nd info (Just . R.Contact $ fromInfo info)

checkClosest :: Node -> Node -> [Node] -> IO ()
checkClosest nd id infos = checkRoutingTable nd id (Just . R.Contacts $ map fromInfo infos)

checkRoutingTable :: Node -> Node -> Maybe R.Result -> IO ()
checkRoutingTable nd id exp = do 
    r <- readTVarIO $ router nd 
    let res = R.find id r 
    res `shouldBe` exp
    pure ()

main :: IO ()
main = do
    nds@(hd:_) <- sequence nodes
    let gs = groups nds
    sequence $ map (forkIO . recvRoutine) nds
    threadDelay 500000
    hspec $ do 
        describe "Node" $ do 
            it "ping a bunch of nodes" $ 
                pings hd (head gs)
            it "checks number of buckets" $
                checkNumberBuckets hd 1
            it "checks contacts" $ 
                checkContacts hd (head gs)
            it "ping another node" $ 
                pingSuccess hd (head $ gs!!1)
            it "checks number of buckets" $ 
                -- should still be 1 bc contacts in same half of range
                checkNumberBuckets hd 1
            it "ping another node" $ 
                pingSuccess hd (head $ last gs)
            it "checks number of buckets" $ 
                checkNumberBuckets hd 2
            it "find contacts" $ 
                findContacts (nds!!1) hd (head gs)
            it "ping some more nodes" $ 
                pings hd $ last gs
            it "find closest" $ do 
                findClosest (reverse $ head gs) (nds!!1) hd (head $ gs!!1)
                findClosest (last gs) (nds!!1) hd (last . last $ init gs)
            it "ping sequence" $ 
                pingSequence $ last gs
            it "iteratively find contact" $ 
                iterFindContact (head $ last gs) [last gs !! 1] (last nds) 
            it "another ping sequence" $ 
                pingSequence . last $ init gs
            it "iteratively find closest" $ 
                iterFindClosest (last $ init gs) (last nds) [last . last $ init gs] hd
            {-
            it "stop node" $ 
                stop $ nds!!1
            it "replace" $ 
                pingSuccess hd $ head $ gs!!1
            it "check routing table" $ do 
                checkContact hd $ head $ gs!!1
                checkClosest hd (nds!!1) ((++[head $ gs!!1]) . remove 1 1 $ head gs)
            -}
