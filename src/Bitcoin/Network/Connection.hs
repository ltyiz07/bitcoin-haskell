{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( connectAndHandshake
    ) where

import Network.Socket
    ( SockAddr(..)
    , Family(AF_INET, AF_INET6)
    , SocketType(Stream)
    , socket
    , connect
    , close
    , defaultProtocol
    )
import Network.Socket.ByteString (sendAll, recv)
import Data.Serialize (runPut, runGet, put, get)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)
import Control.Exception (bracket)

-- 이전에 만든 메시지 모듈 임포트
import Bitcoin.Network.Message

-- | 대상 노드(SockAddr)에 TCP 연결을 맺고 version 메시지를 보내 핸드셰이크를 수행합니다.
connectAndHandshake :: SockAddr -> IO ()
connectAndHandshake addr = do
    putStrLn $ "\n[Connection] 노드에 연결 시도 중... " ++ show addr
    
    -- IPv4인지 IPv6인지에 따라 소켓 패밀리 결정
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET

    -- 소켓 생성 및 안전한 자원 해제를 위해 bracket 패턴 사용 (선택 사항이나 권장됨)
    bracket (socket family Stream defaultProtocol) close $ \sock -> do
        -- 1. TCP 연결
        connect sock addr
        putStrLn "[Connection] TCP 연결 성공!"

        -- 2. 내 version 메시지 생성
        myVersionMsg <- createMyVersion
        
        -- Payload로 감싸고, 다시 Message 헤더로 감싸서 최종 바이트스트림(ByteString) 생성
        let payloadBytes = buildVersionPayload myVersionMsg
            msg = Message (Command "version") payloadBytes
            msgBytes = runPut (put msg)
        -- [디버깅] 전송할 Raw Hex 값을 콘솔에 출력합니다.
        putStrLn $ "[Connection] 'version' 메시지 전송 중... (" ++ show (BS.length msgBytes) ++ " bytes)"
        putStrLn $ "[Debug] 전송되는 Raw Hex:\n" ++ BS8.unpack (B16.encode msgBytes)

        -- 3. version 메시지 전송
        putStrLn "[Connection] 'version' 메시지 전송 중..."
        sendAll sock msgBytes

        -- 4. 응답 대기 (네트워크 버퍼에서 4096 바이트만큼 읽어옴)
        putStrLn "[Connection] 응답 대기 중..."
        respBytes <- recv sock 4096

        if BS.null respBytes
            then putStrLn "[Connection] 연결이 끊어졌거나 응답이 없습니다."
            else do
                -- 5. 받은 응답을 파싱(역직렬화)하여 헤더의 Command 확인
                case runGet get respBytes :: Either String Message of
                    Left err -> putStrLn $ "[Connection] 메시지 파싱 실패: " ++ err
                    Right receivedMsg -> do
                        let cmd = unCommand (msgCommand receivedMsg)
                        putStrLn $ "[Connection] 메시지 수신 성공! Command: " ++ show cmd
                        
                        if cmd == "version" || cmd == "verack"
                            then putStrLn "[Connection] 핸드셰이크가 정상적으로 진행되고 있습니다!"
                            else putStrLn "[Connection] 예상치 못한 메시지를 받았습니다."

-- | 현재 시간과 랜덤 Nonce를 사용하여 기본 VersionMessage를 생성합니다.
createMyVersion :: IO VersionMessage
createMyVersion = do
    -- 현재 UNIX 타임스탬프 (초 단위)
    now <- round <$> getPOSIXTime
    -- 나 자신과의 연결을 방지하기 위한 8바이트 랜덤 숫자
    nonce <- randomIO

    -- 빈 IP와 포트(0)로 구성된 더미 네트워크 주소 생성
    let emptyIp = IPAddress BS.empty 0
        emptyNetAddr = NetworkAddress 0 emptyIp

    return VersionMessage
        { verVersion     = 70015                      -- 지원하는 프로토콜 버전
        , verServices    = 0                          -- 단순 클라이언트이므로 0 (기능 없음)
        , verTimestamp   = now
        , verAddrRecv    = emptyNetAddr               -- 원칙상 상대방 IP를 넣어야 하나, 더미를 넣어도 보통 통과됨
        , verAddrFrom    = emptyNetAddr
        , verNonce       = nonce
        , verUserAgent   = "/ProgrammingBitcoin:0.1/" -- 내 클라이언트 식별자
        , verStartHeight = 0                          -- 내가 가진 최신 블록 높이 (처음엔 0)
        , verRelay       = False                      -- 트랜잭션을 내게 릴레이하지 말라고 요청
        }
