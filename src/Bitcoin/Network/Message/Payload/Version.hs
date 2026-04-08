module Bitcoin.Network.Message.Payload.Version
    ( Version(..)
    , NetworkAddress(..)
    , IPAddress(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import Data.Serialize
    ( Serialize(..)
    , putWord8
    , putWord16be
    , putWord32le
    , putWord64le
    , putByteString
    , getWord8
    , getWord16be
    , getWord32le
    , getWord64le
    , getByteString
    , runPut
    )

import Bitcoin.VarInt (VarInt(..))


data IPAddress = IPAddress 
    { ipHost :: BS.ByteString -- ^ 16 바이트 IP 주소 (IPv4는 IPv4-mapped IPv6 포맷 사용)
    , ipPort :: Word16
    } deriving (Show, Eq)

instance Serialize IPAddress where
    put (IPAddress host port) = do
        putByteString $ BS.take 16 (host <> BS.replicate 16 0)
        putWord16be port
    get = do
        host <- getByteString 16
        port <- getWord16be
        return $ IPAddress host port

data NetworkAddress = NetworkAddress
    { addrServices :: Word64    -- ^ 지원하는 서비스 (예: 1 = NODE_NETWORK)
    , addrIp       :: IPAddress -- ^ IP 주소와 포트 정보 묶음
    } deriving (Show, Eq)

instance Serialize NetworkAddress where
    put addr = do
        putWord64le (addrServices addr)
        put (addrIp addr)
    get = NetworkAddress <$> getWord64le <*> get

data Version = Version
    { version     :: Word32
    , services    :: Word64
    , timestamp   :: Word64
    , addrRecv    :: NetworkAddress
    , addrFrom    :: NetworkAddress
    , nonce       :: Word64
    , userAgent   :: BS.ByteString
    , startHeight :: Word32
    , relay       :: Bool
    } deriving (Show, Eq)

instance Serialize Version where
    put ver = do
        putWord32le ver.version
        putWord64le ver.services
        putWord64le ver.timestamp
        put ver.addrRecv 
        put ver.addrFrom
        putWord64le ver.nonce
        put $ VarInt (fromIntegral $ BS.length ver.userAgent)
        putByteString ver.userAgent
        putWord32le ver.startHeight
        putWord8 (if ver.relay then 1 else 0)
    get = do
        ver   <- getWord32le
        srv   <- getWord64le
        ts    <- getWord64le
        recv  <- get
        from  <- get
        nonce <- getWord64le
        VarInt uaLen <- get
        ua    <- getByteString (fromIntegral uaLen)
        sh    <- getWord32le
        relay <- (== 1) <$> getWord8
        return $ Version ver srv ts recv from nonce ua sh relay
