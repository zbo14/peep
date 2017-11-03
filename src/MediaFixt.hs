module MediaFixt (media) where 

import Data.ByteString.Lazy (readFile)

media = Data.ByteString.Lazy.readFile "/Users/zach/Desktop/peep/src/test.mp3"