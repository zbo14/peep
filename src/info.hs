module Info
(
    Info,
    bytes,
    addr,
    port,
    host,
) where

import Data.ByteString.Lazy (ByteString)
import Id (Id)
import Network.Socket (SockAddr,HostAddress,PortNumber)

class Id a => Info a where 
    addr :: a -> SockAddr
    host :: a -> HostAddress
    port :: a -> PortNumber
    bytes :: a -> ByteString