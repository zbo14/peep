module PeerSpec where 

import Test.Hspec
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import File (File,bitfield)
import Const (pieceSize)
import Conn
import Peer
import ContactFixt (defaultContacts)
import Data.List (find)
import Id (Id,eq)
import IdBytes (IdBytes)
import InfoBytes (InfoBytes)
import TorrentFixt (torrent)
import Value (Dict)
import Util (ExceptIO)
import Data.Either.Utils (maybeToEither)
import Data.Either (isRight,isLeft)
import qualified Chunk as CH

delay = 500000
peers = map Peer.new defaultContacts

serve' :: Peer -> File -> Dict -> IO ()
serve' pr f d = do
    d' <- serve pr (CH.bytes f) "test.mp3" ([] :: [InfoBytes])
    d' `shouldBe` d  
    return ()

torrent' :: Peer -> Dict -> IdBytes -> IO ()
torrent' pr d h = do 
    e <- runExceptT $ Peer.torrent pr d
    e `shouldBe` Right h 
    return ()

findConn :: Id a => Peer -> IdBytes -> a -> ExceptIO Conn
findConn p h i = lookupConns h (conns p) >>= maybeToEither "could not find conn" . find (eq i)

connect' :: Peer -> Peer -> IdBytes -> IO (Conn,Conn)
connect' from to h = do
    e1 <- runExceptT $ connect from to h 
    isRight e1 `shouldBe` True
    let Right cn1 = e1
    eq cn1 to `shouldBe` True
    e2 <- runExceptT $ findConn to h from
    isRight e2 `shouldBe` True
    let Right cn2 = e2
    return (cn1,cn2)

checkBlock :: Peer -> Peer -> IdBytes -> (Int,Int) -> IO ()
checkBlock p1 p2 h (pi,bi) = do
    e1 <- atomically . runExceptT $ lookupBlock h pi bi $ files p1
    isRight e1 `shouldBe` True
    e2 <- atomically . runExceptT $ lookupBlock h pi bi $ files p2
    isRight e2 `shouldBe` True
    let Right (b1,_,_) = e1
        Right (b2,_,_) = e2
    b1 `shouldBe` b2
    return ()

checkPiece :: Peer -> Peer -> IdBytes -> Int -> IO () 
checkPiece p1 p2 h pi = do 
    e1 <- atomically . runExceptT $ lookupPiece h pi $ files p1 
    isRight e1 `shouldBe` True 
    e2 <- atomically . runExceptT $ lookupPiece h pi $ files p2
    isRight e2 `shouldBe` True 
    let Right (pc1,_) = e1 
        Right (pc2,_) = e2
    pc1 `shouldBe` pc2 
    return ()

checkFile :: Peer -> Peer -> IdBytes -> IO () 
checkFile p1 p2 h = do 
    e1 <- atomically $ lookupFile h $ files p1 
    isRight e1 `shouldBe` True 
    e2 <- atomically $ lookupFile h $ files p2
    isRight e2 `shouldBe` True 
    let Right f1 = e1 
        Right f2 = e2
    f1 `shouldBe` f2 
    return ()

unchoke' :: Conn -> Conn -> IO ()
unchoke' cn1 cn2 = do
    atomically $ unchoke cn1
    rc <- readTVarIO $ remoteChoked cn1
    rc `shouldBe` False
    threadDelay delay
    ac <- readTVarIO $ amChoked cn2
    ac `shouldBe` False
    return ()

choke' :: Conn -> Conn -> IO ()
choke' cn1 cn2 = do
    atomically $ choke cn1
    rc <- readTVarIO $ remoteChoked cn1
    rc `shouldBe` True
    threadDelay delay
    ac <- readTVarIO $ amChoked cn2
    ac `shouldBe` True
    return ()

interested' :: Conn -> Conn -> IO ()
interested' cn1 cn2 = do 
    atomically $ interested cn1 
    ai <- readTVarIO $ amInterested cn1
    ai `shouldBe` True 
    threadDelay delay
    ri <- readTVarIO $ remoteInterested cn2
    ri `shouldBe` True 
    return ()

notInterested' :: Conn -> Conn -> IO ()
notInterested' cn1 cn2 = do 
    atomically $ notInterested cn1 
    ai <- readTVarIO $ amInterested cn1
    ai `shouldBe` False
    threadDelay delay
    ri <- readTVarIO $ remoteInterested cn2
    ri `shouldBe` False 
    return ()

readsTChan :: TChan (Int,Bool) -> IO ()
readsTChan tch = forever $ do 
    (pi,res) <- atomically $ readTChan tch
    res `shouldBe` True
    putStrLn $ "got piece: " ++ show pi

requestBlock' :: Conn -> Int -> Int -> IO (Either String ())
requestBlock' cn pi bi = atomically . runExceptT $ requestBlock cn pi bi 

requestPiece' :: Conn -> Int -> IO (Either String ())
requestPiece' cn pi = atomically . runExceptT $ Conn.requestPiece cn pi pieceSize

main :: IO ()
main = do 
    (f,d,h) <- TorrentFixt.torrent
    let Just bf = File.bitfield f
    ps@(pcli:pser:tl) <- sequence peers
    forkIO $ acceptRoutine pser
    hspec $ do 
        describe "Peer" $ do 
            it "serves" $ serve' pser f d
            it "torrents" $ torrent' pcli d h
            it "test conns" $ do 
                (ccli,cser) <- connect' pcli pser h
                atomically $ keepAlive cser
                atomically $ Conn.bitfield cser bf
                threadDelay delay
                has2 <- remoteHas ccli 2
                has2 `shouldBe` True
                unchoke' cser ccli
                interested' ccli cser
                res <- requestBlock' ccli 0 0
                isRight res `shouldBe` True
                threadDelay delay
                checkBlock pcli pser h (0,0)
                choke' cser ccli 
                res <- requestBlock' ccli 0 1
                isLeft res `shouldBe` True
                unchoke' cser ccli 
                notInterested' ccli cser
                res <- requestBlock' ccli 0 1
                isLeft res `shouldBe` True
                interested' ccli cser
                res <- requestPiece' ccli 0 
                isRight res `shouldBe` True 
                threadDelay delay
                has0 <- remoteHas cser 0
                has0 `shouldBe` True
                checkPiece pcli pser h 0
                res <- runExceptT $ Peer.requestPiece pcli h 1
                isRight res `shouldBe` True 
                threadDelay delay 
                has1 <- remoteHas cser 1
                has1 `shouldBe` True 
                checkPiece pcli pser h 1
                eit <- runExceptT $ requestFile pcli h
                isRight eit `shouldBe` True 
                let Right tch = eit
                forkIO $ readsTChan tch
                threadDelay delay
                checkFile pcli pser h

