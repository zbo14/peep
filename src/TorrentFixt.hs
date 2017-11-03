module TorrentFixt (torrent) where 

import Torrent (new)
import InfoBytes (InfoBytes)
import MediaFixt (media)

torrent = media >>= \by -> return $ Torrent.new by "test.mp3" ([] :: [InfoBytes])