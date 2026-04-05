{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Message
    ( IPAddress(..)
    , Command(..)
    , NetworkAddress(..)
    , VersionMessage(..)
    , Message(..)
    , buildVersionPayload
    ) where

import Control.Monad (unless)
import Data.Serialize
    ( Serialize(..)
    , putWord8
    , putWord16be
    , putWord32be
    , putWord32le
    , putWord64le
    , putByteString
    , getWord8
    , getWord16be
    , getWord32be
    , getWord32le
    , getWord64le
    , getByteString
    , runPut
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)

import Utils.Hash (hash256)
import Bitcoin.VarInt (VarInt(..))


data IPAddress = IPAddress 
    { ipHost :: ByteString -- ^ 16 바이트 IP 주소 (IPv4는 IPv4-mapped IPv6 포맷 사용)
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

newtype Command = Command { unCommand :: ByteString }
    deriving (Show, Eq)

instance Serialize Command where
    put (Command cmd) = putByteString $ BS.take 12 (cmd <> BS.replicate 12 0)
    get = do
        bs <- getByteString 12
        return $ Command $ BS.takeWhile (/= 0) bs

data NetworkAddress = NetworkAddress
    { addrServices :: Word64    -- ^ 지원하는 서비스 (예: 1 = NODE_NETWORK)
    , addrIp       :: IPAddress -- ^ IP 주소와 포트 정보 묶음
    } deriving (Show, Eq)

instance Serialize NetworkAddress where
    put addr = do
        putWord64le (addrServices addr)
        put (addrIp addr)
    get = NetworkAddress <$> getWord64le <*> get

data VersionMessage = VersionMessage
    { verVersion     :: Word32
    , verServices    :: Word64
    , verTimestamp   :: Word64
    , verAddrRecv    :: NetworkAddress
    , verAddrFrom    :: NetworkAddress
    , verNonce       :: Word64
    , verUserAgent   :: ByteString -- ^ 클라이언트 버전 문자열
    , verStartHeight :: Word32
    , verRelay       :: Bool       -- ^ 트랜잭션 릴레이 여부 (True = 1, False = 0)
    } deriving (Show, Eq)

instance Serialize VersionMessage where
    put msg = do
        putWord32le (verVersion msg)
        putWord64le (verServices msg)
        putWord64le (verTimestamp msg)
        put (verAddrRecv msg)
        put (verAddrFrom msg)
        putWord64le (verNonce msg)
        put $ VarInt (fromIntegral $ BS.length $ verUserAgent msg)
        putByteString (verUserAgent msg)
        putWord32le (verStartHeight msg)
        putWord8 (if verRelay msg then 1 else 0)

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
        return $ VersionMessage ver srv ts recv from nonce ua sh relay

buildVersionPayload :: VersionMessage -> ByteString
buildVersionPayload = runPut . put

data Message = Message
    { msgCommand :: Command
    , msgPayload :: ByteString
    } deriving (Show, Eq)

-- 비트코인 메인넷 식별자 (testnetMagic: 0x0b110907)
--
data Network = Mainnet | Testnet deriving (Show, Eq)

magicBytes :: Network -> Word32
magicBytes Mainnet = 0xF9BEB4D9
magicBytes Testnet = 0x0B110907

instance Serialize Message where
    put msg = do
        putWord32be (magicBytes Mainnet)
        put (msgCommand msg)
        putWord32le (fromIntegral $ BS.length (msgPayload msg))
        let checksum = BS.take 4 $ hash256 (msgPayload msg)
        putByteString checksum
        putByteString (msgPayload msg)

    get = do
        magic <- getWord32be
        unless (magic == magicBytes Mainnet) $
            fail $ "Invalid magic bytes: expected " ++ show (magicBytes Mainnet) ++ ", got " ++ show magic
        cmd <- get
        len <- getWord32le
        chk <- getByteString 4
        payload <- getByteString (fromIntegral len)
        let expectedChk = BS.take 4 $ hash256 payload
        unless (chk == expectedChk) $
            fail "Invalid checksum in message header"
        return $ Message cmd payload
