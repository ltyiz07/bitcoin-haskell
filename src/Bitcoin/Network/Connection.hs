{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( connectAndHandshake
    ) where

import Network.Socket
    ( Socket
    , SockAddr(..)
    , Family(AF_INET, AF_INET6)
    , SocketType(Stream)
    , socket
    , connect
    , close
    , defaultProtocol
    )
import Network.Socket.ByteString (sendAll, recv)
import Data.Serialize (runPut, put, get)
import Data.Serialize.Get (runGetPartial, Result(..)) -- 버퍼링 파싱을 위해 추가
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)
import Control.Exception (bracket)

-- 이전에 만든 메시지 모듈 임포트
import Bitcoin.Network.Message

-- | 대상 노드(SockAddr)에 TCP 연결을 맺고 양방향 핸드셰이크(version, verack)를 수행합니다.
connectAndHandshake :: SockAddr -> IO ()
connectAndHandshake addr = do
    putStrLn $ "\n[Connection] 노드에 연결 시도 중... " ++ show addr
    
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET

    bracket (socket family Stream defaultProtocol) close $ \sock -> do
        connect sock addr
        putStrLn "[Connection] TCP 연결 성공!"

        ---------------------------------------------------------
        -- 1. 내 version 메시지 전송
        ---------------------------------------------------------
        myVersionMsg <- createMyVersion
        let msg = Message (Command "version") (buildVersionPayload myVersionMsg)
            msgBytes = runPut (put msg)

        putStrLn "[Connection] 'version' 전송 중..."
        sendAll sock msgBytes

        ---------------------------------------------------------
        -- 2. 스트림 파싱 루프 시작 (버퍼링 지원)
        ---------------------------------------------------------
        -- 초기에 빈 바이트스트림으로 파싱을 시작합니다.
        processStream sock False False (runGetPartial get BS.empty)

  where
    -- | TCP 스트림의 파편화/병합을 완벽하게 처리하는 파싱 루프
    processStream :: Socket -> Bool -> Bool -> Result Message -> IO ()
    processStream sock hasVer hasVerack parseResult = do
        if hasVer && hasVerack
            then putStrLn "\n[Connection] 🎉 양방향 핸드셰이크 완벽히 성공! (TCP 소켓 열려 있음)"
            else case parseResult of
                Fail err rest -> do
                    putStrLn $ "[Connection] ❌ 메시지 파싱 실패: " ++ err
                    putStrLn $ "[Connection] 버려진 쓰레기 데이터 길이: " ++ show (BS.length rest)

                Partial feed -> do
                    -- 데이터가 부족하므로 소켓에서 새로 읽어와서 파서에 먹여줍니다(feed).
                    putStrLn "[Connection] 메시지 수신 대기 중..."
                    chunk <- recv sock 4096
                    print chunk 
                    if BS.null chunk
                        then putStrLn "[Connection] ❌ 연결이 끊어졌습니다. (상대방이 소켓을 닫음)"
                        else processStream sock hasVer hasVerack (feed chunk)

                Done msg restBuffer -> do
                    -- 파싱에 성공했으므로 명령어 확인
                    let cmd = BS8.unpack $ unCommand (msgCommand msg)
                    putStrLn $ "[Connection] 📥 메시지 수신: " ++ cmd
                    
                    (nextVer, nextVerack) <- case cmd of
                        "version" -> do
                            putStrLn "[Connection] 📤 'verack' 전송 중..."
                            let verackMsg = Message (Command "verack") BS.empty
                            sendAll sock (runPut $ put verackMsg)
                            return (True, hasVerack)

                        "verack" -> do
                            return (hasVer, True)

                        "ping" -> do
                            -- 연결 유지를 위해 상대방이 보낸 nonce(payload)를 그대로 pong으로 돌려줍니다.
                            putStrLn "[Connection] 📤 'pong' 응답 중..."
                            let pongMsg = Message (Command "pong") (msgPayload msg)
                            sendAll sock (runPut $ put pongMsg)
                            return (hasVer, hasVerack)

                        _ -> do
                            putStrLn $ "[Connection] 무시됨: " ++ cmd
                            return (hasVer, hasVerack)

                    -- ★ 핵심: 파싱하고 남은 찌꺼기 데이터(restBuffer)로 즉시 다음 메시지 파서를 실행합니다.
                    -- 이렇게 하면 한 번의 recv로 들어온 수많은 메시지를 남김없이 처리할 수 있습니다.
                    processStream sock nextVer nextVerack (runGetPartial get restBuffer)


-- | 현재 시간과 랜덤 Nonce를 사용하여 기본 VersionMessage를 생성합니다.
createMyVersion :: IO VersionMessage
createMyVersion = do
    now <- round <$> getPOSIXTime
    nonce <- randomIO

    let emptyIp = IPAddress BS.empty 0
        emptyNetAddr = NetworkAddress 0 emptyIp

    return VersionMessage
        { verVersion     = 70015
        , verServices    = 0
        , verTimestamp   = now
        , verAddrRecv    = emptyNetAddr
        , verAddrFrom    = emptyNetAddr
        , verNonce       = nonce
        , verUserAgent   = "/ProgrammingBitcoin:0.1/"
        , verStartHeight = 0
        , verRelay       = False
        }
