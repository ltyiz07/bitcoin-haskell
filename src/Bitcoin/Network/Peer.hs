{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Peer
    ( runPeer
    ) where

import Data.Word                       (Word32)
import Network.Socket                  (SockAddr)
import Data.Serialize                  (runPut, putWord32le, putWord8, putByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad                   (forever)
import Control.Monad.Except            (ExceptT(..), runExceptT)

import Bitcoin.Network.Message
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage)
import Bitcoin.Network.Handshake       (HandshakeError(..), performHandshake)

-- ---------------------------------------------------------------------------
-- 공개 API
-- ---------------------------------------------------------------------------

-- | 주어진 주소의 노드에 연결하여 핸드셰이크 후 메시지 루프를 실행합니다.
-- 에러는 출력 후 종료합니다.
runPeer :: SockAddr -> IO ()
runPeer addr = do
    result <- runExceptT $ withConnection addr peerSession
    case result of
        Left err -> putStrLn $ "[Peer] 종료: " ++ describeError err
        Right _  -> putStrLn "[Peer] 정상 종료"

-- ---------------------------------------------------------------------------
-- 내부 세션 흐름
-- ---------------------------------------------------------------------------

-- | 연결 후 전체 세션 흐름을 기술합니다.
-- 1. 핸드셰이크  2. 헤더 요청  3. 메시지 루프
peerSession :: NodeConnection -> ExceptT ConnectionError IO ()
peerSession conn = do
    liftHandshake $ performHandshake conn
    sendGetHeaders conn
    peerLoop conn

-- | 핸드셰이크 완료 후 메시지를 무한히 처리합니다.
-- forever 를 사용하여 명시적 재귀를 제거합니다.
-- Left(에러) 수신 시 루프에서 빠져나옵니다.
peerLoop :: NodeConnection -> ExceptT ConnectionError IO ()
peerLoop conn = ExceptT $ do
    result <- runExceptT $ forever $ do
        msg <- recvMessage conn
        handleMessage conn msg
    -- forever 는 Right 를 반환하지 않으므로 Left 만 내려옵니다.
    return result

-- | 단일 메시지를 처리합니다.
-- 새 명령어를 추가할 때 이 함수만 수정하면 됩니다.
handleMessage :: NodeConnection -> Message -> ExceptT ConnectionError IO ()
handleMessage conn msg = do
    let cmd = BS8.unpack $ unCommand (msgCommand msg)
    case cmd of
        "ping" -> do
            liftIO $ putStrLn "[Peer] 수신: ping -> 전송: pong"
            sendMessage conn (Message (Command "pong") (msgPayload msg))

        "headers" -> do
            let payload = msgPayload msg
            liftIO $ do
                putStrLn $ "[Peer] 📦 블록 헤더 수신! (" ++ show (BS.length payload) ++ " bytes)"
                -- TODO: raw 덤프 대신 BlockHeader 파싱으로 교체 필요
                BS.writeFile "res_headers.dat" payload

        _ ->
            liftIO $ putStrLn $ "[Peer] 수신 (무시됨): " ++ cmd

-- ---------------------------------------------------------------------------
-- 메시지 생성
-- ---------------------------------------------------------------------------

-- | getheaders 메시지를 전송합니다.
-- 제네시스 블록 해시를 Locator 로 지정하여 이후 2000개 헤더를 요청합니다.
sendGetHeaders :: NodeConnection -> ExceptT ConnectionError IO ()
sendGetHeaders conn = do
    let payload = runPut $ do
                putWord32le protocolVersion
                putWord8 1                          -- Locator 해시 개수
                putByteString genesisHashLE         -- 시작 해시 (little-endian)
                putByteString (BS.replicate 32 0)   -- 종료 해시 (0 = 끝까지)
        msg = Message (Command "getheaders") payload
    sendMessage conn msg
    liftIO $ putStrLn "[Peer] 🚀 전송: getheaders (제네시스 블록 이후 2000개 요청)"

-- ---------------------------------------------------------------------------
-- 상수
-- ---------------------------------------------------------------------------

protocolVersion :: Word32
protocolVersion = 70015

-- | 제네시스 블록 해시 (little-endian 직렬화)
genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]

-- ---------------------------------------------------------------------------
-- 헬퍼
-- ---------------------------------------------------------------------------

-- | HandshakeError 를 ConnectionError 로 변환합니다.
-- 에러 계층을 한 방향으로 흘려 보냅니다.
liftHandshake :: ExceptT HandshakeError IO a -> ExceptT ConnectionError IO a
liftHandshake = ExceptT . fmap (either (Left . toConnError) Right) . runExceptT
  where
    toConnError HandshakeTimeout          = ConnectFailed "핸드셰이크 타임아웃"
    toConnError (HandshakeConnError e)    = e
    toConnError (HandshakeFailed reason)  = ConnectFailed ("핸드셰이크 실패: " ++ reason)

-- | IO 액션을 ExceptT ConnectionError IO 로 승격시킵니다.
liftIO :: IO a -> ExceptT ConnectionError IO a
liftIO = ExceptT . fmap Right

-- | 에러를 사람이 읽을 수 있는 문자열로 변환합니다.
describeError :: ConnectionError -> String
describeError (ConnectFailed reason) = "연결 실패 - " ++ reason
describeError (ParseError msg)       = "파싱 에러 - " ++ msg
describeError ConnectionClosed       = "상대방이 연결을 닫음"
