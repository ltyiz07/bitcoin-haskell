{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Handshake
    ( HandshakeError(..)
    , performHandshake
    ) where

import Data.Word                       (Word64)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX           (getPOSIXTime)
import System.Random                   (randomIO)
import System.Timeout                  (timeout)
import Control.Monad.Except            (ExceptT(..), runExceptT)

import Bitcoin.Network.Message
import Bitcoin.Network.Connection      (NodeConnection, ConnectionError, sendMessage, recvMessage)


data HandshakeError
    = HandshakeTimeout                    -- 제한 시간 내에 핸드셰이크 미완료
    | HandshakeConnError ConnectionError  -- 하위 연결 레이어 에러
    | HandshakeFailed    String           -- 예상치 못한 프로토콜 위반
    deriving (Show, Eq)

data HandshakeState = HandshakeState
    { hsGotVersion :: Bool  -- 상대방의 version 메시지 수신 여부
    , hsGotVerAck  :: Bool  -- 상대방의 verack 메시지 수신 여부
    } deriving (Show)

initialState :: HandshakeState
initialState = HandshakeState { hsGotVersion = False, hsGotVerAck = False }

isComplete :: HandshakeState -> Bool
isComplete (HandshakeState True True) = True
isComplete _                          = False

-- | 핸드셰이크 타임아웃 (마이크로초, 10초)
handshakeTimeoutUs :: Int
handshakeTimeoutUs = 10 * 1_000_000

performHandshake :: NodeConnection -> ExceptT HandshakeError IO ()
performHandshake conn = ExceptT $ do
    result <- timeout handshakeTimeoutUs $ runExceptT doHandshake
    case result of
        Nothing -> do
            putStrLn "[Handshake] 타임아웃: 응답 없음"
            return $ Left HandshakeTimeout
        Just r  ->
            return r
  where
    doHandshake :: ExceptT HandshakeError IO ()
    doHandshake = do
        sendVersion conn
        loop conn initialState

loop :: NodeConnection -> HandshakeState -> ExceptT HandshakeError IO ()
loop conn state
    | isComplete state =
        liftIO $ putStrLn "[Handshake] 🎉 양방향 핸드셰이크 완료!"
    | otherwise = do
        msg <- liftConn $ recvMessage conn
        let cmd = BS8.unpack $ unCommand (msgCommand msg)
        liftIO $ putStrLn $ "[Handshake] 수신: " ++ cmd
        nextState <- handleCmd conn cmd msg state
        loop conn nextState

-- | 수신 명령어에 따라 응답을 보내고 상태를 갱신합니다.
handleCmd
    :: NodeConnection
    -> String
    -> Message
    -> HandshakeState
    -> ExceptT HandshakeError IO HandshakeState
handleCmd conn "version" _   st = do
    liftConn $ sendMessage conn (Message (Command "verack") BS.empty)
    liftIO $ putStrLn "[Handshake] 전송: verack"
    return st { hsGotVersion = True }

handleCmd _    "verack"  _   st =
    return st { hsGotVerAck = True }

handleCmd conn "ping"    msg st = do
    liftConn $ sendMessage conn (Message (Command "pong") (msgPayload msg))
    liftIO $ putStrLn "[Handshake] 전송: pong"
    return st

handleCmd _    other     _   st = do
    liftIO $ putStrLn $ "[Handshake] 무시됨: " ++ other
    return st

sendVersion :: NodeConnection -> ExceptT HandshakeError IO ()
sendVersion conn = do
    now   <- liftIO $ (round :: Double -> Word64) . realToFrac <$> getPOSIXTime
    nonce <- liftIO (randomIO :: IO Word64)
    let emptyAddr = NetworkAddress 0 (IPAddress (BS.replicate 16 0x00) 0)
        myVer     = VersionMessage
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
    liftConn $ sendMessage conn (Message (Command "version") (buildVersionPayload myVer))
    liftIO $ putStrLn "[Handshake] 전송: version"

liftConn :: ExceptT ConnectionError IO a -> ExceptT HandshakeError IO a
liftConn = ExceptT . fmap (either (Left . HandshakeConnError) Right) . runExceptT

liftIO :: IO a -> ExceptT HandshakeError IO a
liftIO = ExceptT . fmap Right
