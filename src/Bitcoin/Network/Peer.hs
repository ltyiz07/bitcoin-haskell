{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Peer
    ( runPeer
    ) where

import Network.Socket (SockAddr)
import Data.Serialize (runPut, putWord32le, putWord8, putByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Bitcoin.Network.Message
import Bitcoin.Network.Connection (NodeConnection, withConnection, sendMessage, recvMessage)
import Bitcoin.Network.Handshake (performHandshake)

runPeer :: SockAddr -> IO ()
runPeer addr = withConnection addr $ \conn -> do
    -- 1. 인증 대기
    success <- performHandshake conn
    
    if success
        then do
            -- 2. 헤더 요청
            sendGetHeaders conn
            -- 3. 메인 무한 루프 시작
            peerLoop conn
        else 
            putStrLn "[Peer] 핸드셰이크 실패로 통신을 종료합니다."

  where
    -- | 인증 완료 후 일반적인 메시지 처리 무한 루프
    peerLoop :: NodeConnection -> IO ()
    peerLoop conn = do
        result <- recvMessage conn
        case result of
            Left err -> 
                putStrLn $ "[Peer] 통신 종료: " ++ err
            Right (msg :: Message) -> do
                let cmd = BS8.unpack $ unCommand (msgCommand msg)
                
                case cmd of
                    "ping" -> do
                        putStrLn "[Peer] 수신: ping -> 전송: pong"
                        sendMessage conn (Message (Command "pong") (msgPayload msg))
                    "headers" -> do
                        let payload = msgPayload msg
                        putStrLn $ "[Peer] 📦 블록 헤더 수신 완료! (" ++ show (BS.length payload) ++ " bytes)"
                        BS.writeFile "res_headers.dat" payload
                    _ -> 
                        putStrLn $ "[Peer] 수신 (무시됨): " ++ cmd
                
                -- 다음 메시지를 기다림
                peerLoop conn

    sendGetHeaders :: NodeConnection -> IO ()
    sendGetHeaders conn = do
        let payload = runPut $ do
                putWord32le 70015
                putWord8 1
                putByteString genesisHashLE
                putByteString (BS.replicate 32 0)
            msg = Message (Command "getheaders") payload
            
        sendMessage conn msg
        putStrLn "[Peer] 🚀 전송: getheaders (제네시스 블록 이후 2000개 요청)"

    genesisHashLE :: BS.ByteString
    genesisHashLE = BS.pack
        [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
        , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
        , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
        , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
        ]
