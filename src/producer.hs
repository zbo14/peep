module Producer
(
    Producer,
    one,
    many
)
where 

import Chunk (Chunk,defaultSize)
import Data.ByteString.Lazy (ByteString)
import Data.List (reverse)
import Util (bLength,bSplitAt)

class Chunk a => Producer a where 
    one :: ByteString -> Int -> a
    many :: ByteString -> [a]
    many = many' [] 0
    many' :: [a] -> Int -> ByteString -> [a]
    many' acc _ b | bLength b == 0 = reverse acc
    many' acc idx b = many' (x:acc) (idx+1) rst 
        where (b',rst) = bSplitAt (defaultSize $ head acc) b
              x = one b' idx