{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Handshake
    ( performHandshake
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)

import Bitcoin.Network.Message
import Bitcoin.Network.Connection (NodeConnection, sendMessage, recvMessage)

-- | 연결된 노드와 Handshake를 수행합니다. 
performHandshake :: NodeConnection -> IO Bool
performHandshake conn = do
    sendMyVersion conn
    -- 핸드셰이크 상태 머신 시작 (hasVer = False, hasVerAck = False)
    loop False False
  where
    loop :: Bool -> Bool -> IO Bool
    loop True True = do
        putStrLn "[Handshake] 🎉 양방향 핸드셰이크 완벽히 성공!"
        return True
    loop hasVer hasVerAck = do
        -- 메시지 하나를 달라고 요청 (버퍼 관리는 알아서 해줌)
        result <- recvMessage conn
        case result of
            Left err -> do
                putStrLn $ "[Handshake] 실패: " ++ err
                return False
            Right (msg :: Message) -> do
                let cmd = BS8.unpack $ unCommand (msgCommand msg)
                putStrLn $ "[Handshake] 수신: " ++ cmd
                
                case cmd of
                    "version" -> do
                        sendMessage conn (Message (Command "verack") BS.empty)
                        putStrLn "[Handshake] 전송: verack"
                        loop True hasVerAck
                        
                    "verack" -> 
                        loop hasVer True
                        
                    "ping" -> do
                        sendMessage conn (Message (Command "pong") (msgPayload msg))
                        putStrLn "[Handshake] 전송: pong"
                        loop hasVer hasVerAck
                        
                    _ -> do
                        putStrLn $ "[Handshake] 무시됨: " ++ cmd
                        loop hasVer hasVerAck

    sendMyVersion :: NodeConnection -> IO ()
    sendMyVersion conn = do
        now   <- round <$> getPOSIXTime
        nonce <- randomIO
        let emptyAddr = NetworkAddress 0 (IPAddress (BS.replicate 16 0x00) 0)
        let myVer = VersionMessage
                { verVersion     = 70015
                , verServices    = 0
                , verTimestamp   = now
                , verAddrRecv    = emptyAddr
                , verAddrFrom    = emptyAddr
                , verNonce       = nonce
                , verUserAgent   = "/ProgrammingBitcoin:0.1/"
                , verStartHeight = 0
                , verRelay       = False
                }
        sendMessage conn (Message (Command "version") (buildVersionPayload myVer))
        putStrLn "[Handshake] 전송: version"
