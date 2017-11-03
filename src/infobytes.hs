module InfoBytes 
(
    InfoBytes,
    new,
    fromAddrId
) where 

import Const (idSize,infoSize)
import Data.ByteString.Lazy (ByteString,index,length,pack,take)
import Util (encodeWord16,decodeWord16,portNum)
import Data.Word (Word8,Word16)
import Id
import Info
import Network.Socket (SockAddr(SockAddrInet),HostAddress,PortNumber,hostAddressToTuple,tupleToHostAddress)

newtype InfoBytes = InfoBytes ByteString

new :: ByteString -> Maybe InfoBytes
new b | Data.ByteString.Lazy.length b < fromIntegral infoSize = Nothing 
new b = Just . InfoBytes $ Data.ByteString.Lazy.take (fromIntegral infoSize) b

encode :: Id a => a -> Word16 -> (Word8,Word8,Word8,Word8) -> ByteString
encode i p (h1,h2,h3,h4) = 
    (Id.bytes i `mappend`) . Data.ByteString.Lazy.pack $ [h1,h2,h3,h4] ++ encodeWord16 p

bytesToHostAddress :: ByteString -> HostAddress
bytesToHostAddress b = tupleToHostAddress (index b 20,index b 21,index b 22,index b 23)

bytesToPortNumber :: ByteString -> PortNumber
bytesToPortNumber b = portNum $ decodeWord16 (index b 24,index b 25)

fromAddrId :: Id a => SockAddr -> a -> InfoBytes
fromAddrId a@(SockAddrInet p h) i = InfoBytes $ encode i (fromIntegral p) (hostAddressToTuple h)

instance Id InfoBytes where
    bytes = Data.ByteString.Lazy.take (fromIntegral idSize) . Info.bytes

instance Info InfoBytes where
    addr info = SockAddrInet (port info) (host info)
    port = bytesToPortNumber . Info.bytes
    bytes (InfoBytes b) = b
    host = bytesToHostAddress . Info.bytes