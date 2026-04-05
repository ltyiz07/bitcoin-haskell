{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Handshake
    ( performHandshake
    ) where

import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import Data.Serialize (runPut, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)
import Data.IORef

import Bitcoin.Network.Message
import Bitcoin.Network.Connection (runMessageLoop)

-- | 성공 시 '남은 버퍼(Leftover Buffer)'를 반환합니다.
performHandshake :: Socket -> IO (Either String BS.ByteString)
performHandshake sock = do
    hasVerRef <- newIORef False
    hasVerAckRef <- newIORef False
    
    sendMyVersion sock
    
    -- 초기 버퍼는 비어있는 상태로 시작
    result <- runMessageLoop sock BS.empty (handshakeHandler sock hasVerRef hasVerAckRef)
    
    case result of
        Left err -> do
            putStrLn $ "[Handshake] 실패: " ++ err
            return $ Left err
        Right leftover -> do
            putStrLn "[Handshake] 🎉 양방향 핸드셰이크 완벽히 성공!"
            return $ Right leftover

  where
    handshakeHandler :: Socket -> IORef Bool -> IORef Bool -> Message -> IO Bool
    handshakeHandler sock hasVerRef hasVerAckRef msg = do
        let cmd = BS8.unpack $ unCommand (msgCommand msg)
        putStrLn $ "[Handshake] 수신: " ++ cmd
        
        case cmd of
            "version" -> do
                sendAll sock (runPut $ put $ Message (Command "verack") BS.empty)
                putStrLn "[Handshake] 전송: verack"
                writeIORef hasVerRef True
            "verack" -> do
                writeIORef hasVerAckRef True
            "ping" -> do
                -- ★ 핸드셰이크 도중에도 ping이 오면 튕겨내지 않고 반드시 pong을 쳐줍니다!
                sendAll sock (runPut $ put $ Message (Command "pong") (msgPayload msg))
                putStrLn "[Handshake] 전송: pong"
            _ -> 
                putStrLn $ "[Handshake] 무시됨 (핸드셰이크 중): " ++ cmd
        
        v <- readIORef hasVerRef
        va <- readIORef hasVerAckRef
        
        return (not (v && va))

    sendMyVersion :: Socket -> IO ()
    sendMyVersion sock = do
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
        let msg = Message (Command "version") (runPut $ put myVer)
        sendAll sock (runPut $ put msg)
        putStrLn "[Handshake] 전송: version"
