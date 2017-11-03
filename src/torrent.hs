module Torrent 
(
    Torrent.new,
    torrent
) where 


import Const (idSize,pieceSize)
import Bencode(encode)
import File (File,empty,new,pieces,size)
import IdBytes (IdBytes,new,hash)
import Info (Info,bytes)
import Piece (empty,hash)
import Util (chunkBytes)
import Value(Value(D),Dict,newDict,dictLookup,wrap,unwrap)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Data.ByteString.Lazy (ByteString,concat)
import Data.Either.Utils (maybeToEither)

pl = wrap pieceSize

infoHash :: Dict -> IdBytes
infoHash d = IdBytes.hash . encode $ D d

new :: Info a => ByteString -> String -> [a] -> (File,Dict,IdBytes)
new by nm nds = (f,d,h)
    where f = File.new by nm
          l = wrap $ size f
          pcs = wrap . Data.ByteString.Lazy.concat $ map Piece.hash $ pieces f 
          info = newDict [("name",wrap $ pack nm),("piece_length",pl),("pieces",pcs),("length",l)]
          h = infoHash info
          nds' = wrap $ map (wrap . bytes) nds
          d = newDict [("info",wrap info),("nodes",nds')]

lookupInfo :: Dict -> Either String Dict 
lookupInfo d = maybeToEither "could not find torrent info" (dictLookup "info" d >>= unwrap :: Maybe Dict) 

lookupLength :: Dict -> Either String Int 
lookupLength d = maybeToEither "could not find length" (dictLookup "length" d >>= unwrap :: Maybe Int)

lookupName :: Dict -> Either String ByteString 
lookupName d = maybeToEither "could not find name" (dictLookup "name" d >>= unwrap :: Maybe ByteString)

lookupPieces :: Dict -> Either String [IdBytes]
lookupPieces d = do 
    pcs <- maybeToEither "could not find pieces" (dictLookup "pieces" d >>= unwrap :: Maybe ByteString)
    maybeToEither "could not get [IdBytes] from pieces" (sequence . map IdBytes.new $ chunkBytes idSize pcs)

torrent :: Dict -> Either String (File,IdBytes) 
torrent d = do 
    info <- lookupInfo d
    let h = infoHash info
    sz <- lookupLength info
    nm <- lookupName info
    hs <- lookupPieces info
    let md = sz `mod` pieceSize
        szs = let szs' = replicate (sz `div` pieceSize) pieceSize in if md == 0 then szs' else szs'++[md]
        pcs = foldr (\(h,idx,sz) acc -> (Piece.empty h idx sz) : acc) [] (zip3 hs [0..] szs)
        f = File.empty pcs (unpack nm) sz
    return $ (f,h) 