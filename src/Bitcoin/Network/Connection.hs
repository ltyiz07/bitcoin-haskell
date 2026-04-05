{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( connectAndHandshake
    ) where

import Network.Socket
    ( Socket, SockAddr(..), Family(AF_INET, AF_INET6)
    , SocketType(Stream), socket, connect, close, defaultProtocol
    )
import Network.Socket.ByteString (sendAll, recv)
import Data.Serialize            (runPut, put, get)
import Data.Serialize.Get        (runGetPartial, Result(..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random         (randomIO)
import Control.Exception     (bracket)
import Control.Monad         (when)

import Bitcoin.Network.Message


connectAndHandshake :: SockAddr -> IO ()
connectAndHandshake addr = do
    putStrLn $ "[Connection] 연결 시도: " ++ show addr
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET
    bracket (socket family Stream defaultProtocol) close $ \sock -> do
        connect sock addr
        putStrLn "[Connection] TCP 연결 성공"
        sendVersion sock
        result <- runConnectionLoop sock
        case result of
            Left  err      -> putStrLn $ "[Connection] 연결 종료: " ++ err
            Right restByte -> putStrLn   "[Connection] 통신 종료"
        putStrLn "[Connection] 🎉 핸드셰이크 완료"
  where
    runConnectionLoop :: Socket -> IO (Either String BS.ByteString)
    runConnectionLoop sock = go False False (runGetPartial get BS.empty)
      where
        go :: Bool -> Bool -> Result Message -> IO (Either String BS.ByteString)
        go True True parseState = return $ Right parseState
        go hasVer hasVerack parseState = case parseState of
            Fail err _ ->
                return $ Left err
            Partial feed -> do
                chunk <- recv sock 4096
                if BS.null chunk
                    then return $ Left "연결 끊김 (상대방이 소켓을 닫음)"
                    else go hasVer hasVerack (feed chunk)
            Done msg rest -> do
                (nextVer, nextVerack) <- handleConnection sock msg hasVer hasVerack
                go nextVer nextVerack parseState

    handleConnection :: Socket -> Message -> Bool -> Bool -> IO (Bool, Bool)
    handleConnection sock msg hasVer hasVerack = do
        let cmd = BS8.unpack $ unCommand (msgCommand msg)
        putStrLn $ "[Connection] 수신: " ++ cmd
        case cmd of
            "version" -> do
                sendAll sock (runPut $ put $ Message (Command "verack") BS.empty)
                putStrLn "[Connection] 전송: verack"
                return (True, hasVerack)
            "verack"  ->
                return (hasVer, True)
            "ping"    -> do
                sendAll sock (runPut $ put $ Message (Command "pong") (msgPayload msg))
                putStrLn "[Connection] 전송: pong"
                return (hasVer, hasVerack)
            _         ->
                return (hasVer, hasVerack)

    sendVersion :: Socket -> IO ()
    sendVersion sock = do
        myVer <- createMyVersion
        let msg = Message (Command "version") (runPut $ put myVer)
        sendAll sock (runPut $ put msg)
        putStrLn "[Connection] 전송: version"

    createMyVersion :: IO VersionMessage
    createMyVersion = do
        now   <- round <$> getPOSIXTime
        nonce <- randomIO
        let emptyAddr = NetworkAddress 0 (IPAddress (BS.replicate 16 0x00) 0)
        return VersionMessage
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
