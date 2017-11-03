module Contact
(
    Contact,
    new,
    fromInfo,
    host,
    port,
    addr,
    lastHeard,
    isBad,
    notBad,
    bad,
    heard
) where

import Const (idSize)
import Data.Word (Word8,Word16)
import Network.Socket (HostAddress,PortNumber,SockAddr(SockAddrInet),tupleToHostAddress)
import Data.ByteString.Lazy (ByteString,take)
import qualified Id
import qualified Info
import InfoBytes (fromAddrId)
import Util (now,portNum)

data Contact = Contact {
    addr :: SockAddr,
    host :: HostAddress,
    port :: PortNumber,
    bytes :: ByteString,
    lastHeard :: IO Int,
    isBad :: Bool
}

notBad :: Contact -> Contact
notBad Contact{addr=a,host=h,port=p,bytes=b,lastHeard=tm} =
    Contact {
        addr=a, 
        host=h, 
        port=p, 
        bytes=b,
        lastHeard=tm,
        isBad=False
    }

bad :: Contact -> Contact
bad Contact{addr=a,host=h,port=p,bytes=b,lastHeard=tm} =
    Contact {
        addr=a, 
        host=h, 
        port=p, 
        bytes=b,
        lastHeard=tm,
        isBad=True
    }

heard :: Contact -> Contact
heard Contact{addr=a,host=h,port=p,bytes=b,isBad=ib} =
    Contact {
        addr=a, 
        host=h, 
        port=p, 
        bytes=b,
        lastHeard=now,
        isBad=ib
    }

fromInfo :: Info.Info a => a -> Contact
fromInfo i =
    Contact {
        addr=Info.addr i, 
        host=Info.host i, 
        port=Info.port i, 
        bytes=Info.bytes i,
        lastHeard=now,
        isBad=False
    }

new :: Id.Id a => a -> Word16 -> (Word8,Word8,Word8,Word8) -> Contact
new i p h =
    Contact {
        addr=a, 
        host=Info.host i', 
        port=Info.port i',
        bytes=Info.bytes i',
        lastHeard=now,
        isBad=False
    }
    where h' = tupleToHostAddress h
          p' = portNum p
          a =  SockAddrInet p' h'
          i'= fromAddrId a i

instance Info.Info Contact where 
    addr = addr 
    host = host 
    port = port
    bytes = bytes

instance Id.Id Contact where
    bytes = Data.ByteString.Lazy.take (fromIntegral idSize) . Contact.bytes

instance Ord Contact where 
    c1 `compare` c2 = Id.cmp c1 c2

instance Eq Contact where 
    Contact {bytes=b1} == Contact {bytes=b2} = b1 == b2

instance Show Contact where 
    show c@Contact {addr=a} = "CONTACT {ADDR='" ++ show a ++ "', ID=" ++ (show $ Id.bytes c) ++ "}"
