module IdBytes 
(
    IdBytes,
    IdBytes.new,
    hash,
    rand
) where 

import Data.ByteString.Lazy (ByteString,length,pack,take)
import Const (idSize)
import Id 
import System.Random (randomRs,getStdGen)
import Util (sha1)

newtype IdBytes = IdBytes ByteString deriving (Show,Eq)

new :: ByteString -> Maybe IdBytes
new b | fromIntegral (Data.ByteString.Lazy.length b) < idSize = Nothing 
new b = Just . IdBytes $ Data.ByteString.Lazy.take (fromIntegral idSize) b

hash :: ByteString -> IdBytes
hash = IdBytes . sha1

rand :: IO IdBytes
rand = getStdGen >>= return . IdBytes . Data.ByteString.Lazy.pack . Prelude.take idSize . randomRs (0x0, 0xFF)

instance Id IdBytes where
    bytes (IdBytes b) = b