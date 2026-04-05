{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Peer
    ( runPeer
    ) where

import Network.Socket (Socket, SockAddr)
import Network.Socket.ByteString (sendAll)
import Data.Serialize (runPut, put, putWord32le, putWord8, putByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Bitcoin.Network.Message
import Bitcoin.Network.Connection (withConnection, runMessageLoop)
import Bitcoin.Network.Handshake (performHandshake)

runPeer :: SockAddr -> IO ()
runPeer addr = withConnection addr $ \sock -> do
    handshakeResult <- performHandshake sock
    
    case handshakeResult of
        Right leftoverBuffer -> do
            -- 핸드셰이크 성공 직후 getheaders 요청 발송
            sendGetHeaders sock
            
            -- ★ 중요: Handshake가 끝난 시점에 남겨진 '찌꺼기 버퍼(leftoverBuffer)'를 그대로 이어받아 메인 루프 시작!
            result <- runMessageLoop sock leftoverBuffer (peerHandler sock)
            
            case result of
                Left err -> putStrLn $ "[Peer] 노드와 연결 끊어짐: " ++ err
                Right _  -> putStrLn "[Peer] 통신 정상 종료"
        Left err -> 
            putStrLn "[Peer] 핸드셰이크 실패로 통신을 종료합니다."

  where
    peerHandler :: Socket -> Message -> IO Bool
    peerHandler sock msg = do
        let cmd = BS8.unpack $ unCommand (msgCommand msg)
        
        case cmd of
            "ping" -> do
                putStrLn "[Peer] 수신: ping -> 전송: pong"
                sendAll sock (runPut $ put $ Message (Command "pong") (msgPayload msg))
            "headers" -> do
                let payload = msgPayload msg
                putStrLn $ "[Peer] 📦 블록 헤더 수신 완료! (크기: " ++ show (BS.length payload) ++ " bytes)"
                BS.writeFile "data/res_headers.dat" payload
                putStrLn "[Peer] 💾 'res_headers.dat' 파일 저장 완료."
            _ -> 
                -- 무시되는 메시지는 페이로드 크기라도 로그에 찍어둡니다. (디버깅 용이)
                putStrLn $ "[Peer] 수신 (무시됨): " ++ cmd ++ " (Payload: " ++ show (BS.length (msgPayload msg)) ++ " bytes)"
        
        return True

    sendGetHeaders :: Socket -> IO ()
    sendGetHeaders sock = do
        let payload = runPut $ do
                putWord32le 70015
                putWord8 1
                putByteString genesisHashLE
                putByteString (BS.replicate 32 0)
            msg = Message (Command "getheaders") payload
            
        sendAll sock (runPut $ put msg)
        putStrLn "[Peer] 🚀 전송: getheaders (제네시스 블록 이후 2000개 요청)"

    genesisHashLE :: BS.ByteString
    genesisHashLE = BS.pack
        [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
        , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
        , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
        , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
        ]
