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

--------------------------------------------------------------------------------
-- 0. 공통 커스텀 타입 (Newtypes)
--------------------------------------------------------------------------------

-- | 16바이트로 고정되는 IP 주소와 포트를 함께 묶어 관리하는 타입
data IPAddress = IPAddress 
    { ipHost :: ByteString -- ^ 16 바이트 IP 주소 (IPv4는 IPv4-mapped IPv6 포맷 사용)
    , ipPort :: Word16     -- ^ 포트 번호
    } deriving (Show, Eq)

instance Serialize IPAddress where
    put (IPAddress host port) = do
        -- 1. IP 주소를 무조건 16바이트로 맞춤 (부족하면 0으로 채움)
        putByteString $ BS.take 16 (host <> BS.replicate 16 0)
        -- 2. 포트 번호는 네트워크 바이트 오더(**Big-Endian**)를 사용함에 주의!
        putWord16be port
        
    get = do
        host <- getByteString 16
        port <- getWord16be
        return $ IPAddress host port

-- | 12바이트로 고정되는 명령어 타입 (예: "version")
newtype Command = Command { unCommand :: ByteString }
    deriving (Show, Eq)

instance Serialize Command where
    -- 명령어를 무조건 12바이트로 맞춤 (부족하면 0으로 채움)
    put (Command cmd) = putByteString $ BS.take 12 (cmd <> BS.replicate 12 0)
    get = do
        bs <- getByteString 12
        -- 읽어올 때는 널(0x00) 패딩을 제거하고 순수 문자열만 남깁니다.
        return $ Command $ BS.takeWhile (/= 0) bs

--------------------------------------------------------------------------------
-- 1. 네트워크 주소 (Network Address)
--------------------------------------------------------------------------------

-- | 비트코인 노드의 주소 정보를 담는 구조체 (Services + IP/Port)
data NetworkAddress = NetworkAddress
    { addrServices :: Word64    -- ^ 지원하는 서비스 (예: 1 = NODE_NETWORK)
    , addrIp       :: IPAddress -- ^ IP 주소와 포트 정보 묶음
    } deriving (Show, Eq)

instance Serialize NetworkAddress where
    put addr = do
        putWord64le (addrServices addr)
        put (addrIp addr)
    get = NetworkAddress <$> getWord64le <*> get

--------------------------------------------------------------------------------
-- 2. Version 메시지 페이로드 (Version Payload)
--------------------------------------------------------------------------------

-- | 노드 연결 시 가장 먼저 주고받는 version 메시지 구조체
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
        
        -- Transaction 모듈의 scriptPubKey처럼, 문자열 앞에는 항상 VarInt로 길이를 표기해야 합니다.
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
        
        -- VarInt를 읽어 User Agent 문자열의 길이를 파악한 후 파싱합니다.
        VarInt uaLen <- get
        ua    <- getByteString (fromIntegral uaLen)
        
        sh    <- getWord32le
        relay <- (== 1) <$> getWord8
        return $ VersionMessage ver srv ts recv from nonce ua sh relay

-- | VersionMessage 객체를 실제 바이트스트림으로 변환합니다.
buildVersionPayload :: VersionMessage -> ByteString
buildVersionPayload = runPut . put

--------------------------------------------------------------------------------
-- 3. 공통 메시지 헤더 (Message Header)
--------------------------------------------------------------------------------

-- | 모든 P2P 메시지를 감싸는 공통 래퍼 (Header + Payload)
data Message = Message
    { msgCommand :: Command    -- ^ 명령어 문자열 (커스텀 타입 적용)
    , msgPayload :: ByteString -- ^ 직렬화된 페이로드 바이트스트림
    } deriving (Show, Eq)

-- 비트코인 메인넷 식별자 (testnetMagic: 0x0b110907)
mainnetMagic :: Word32
mainnetMagic = 0xF9BEB4D9

instance Serialize Message where
    put msg = do
        putWord32be mainnetMagic
        put (msgCommand msg)
        putWord32le (fromIntegral $ BS.length (msgPayload msg))
        let checksum = BS.take 4 $ hash256 (msgPayload msg)
        putByteString checksum
        putByteString (msgPayload msg)

    get = do
        -- 1. 매직 바이트 검증 (Transaction의 SegWit Marker 검증과 동일한 패턴)
        magic <- getWord32be
        unless (magic == mainnetMagic) $
            fail $ "Invalid magic bytes: expected " ++ show mainnetMagic ++ ", got " ++ show magic
        
        -- 2. 명령어 파싱
        cmd <- get
        
        -- 3. 페이로드 길이 및 체크섬 확인
        len <- getWord32le
        chk <- getByteString 4
        
        -- 4. 페이로드 데이터 읽기 및 체크섬 무결성 검증
        payload <- getByteString (fromIntegral len)
        let expectedChk = BS.take 4 $ hash256 payload
        unless (chk == expectedChk) $
            fail "Invalid checksum in message header"
            
        return $ Message cmd payload
