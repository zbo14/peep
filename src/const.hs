module Const
(
    protocolName,
    numReservedBytes,
    blockSize,
    bucketSize,
    idSize,
    infoSize,
    pieceSize,
    recvSize,
    bucketTimeout,
    messageTimeout,
    nodeTimeout,
    refreshInterval,
    chokingInterval
) where
    
protocolName = "BitTorrent protocol"
numReservedBytes = 8 :: Int

blockSize = 16384 :: Int
bucketSize = 8 :: Int
idSize = 20 :: Int
infoSize = 26 :: Int
pieceSize = 262144 :: Int
recvSize = 4096 :: Int

bucketTimeout = 30000000 :: Int
messageTimeout = 10000000 :: Int
nodeTimeout = 60000000 :: Int
refreshInterval = 30000000 :: Int
chokingInterval = 10000000 :: Int