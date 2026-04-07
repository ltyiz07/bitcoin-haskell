{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.Handshake
    ( HandshakeError(..)
    , performHandshake
    , liftConnectionException
    , liftHandshakeException 
    ) where

import Data.Word                       (Word64)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock.POSIX           (getPOSIXTime)
import System.Random                   (randomIO)
import System.Timeout                  (timeout)
import Control.Monad.Except            (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class          (liftIO)
import Data.Serialize                  (Serialize(..), runGet)

import Bitcoin.Network.Message.Header
import Bitcoin.Network.Message.Payload.Version
import Bitcoin.Network.Connection      (NodeConnection, ConnectionError(..), sendMessage, recvMessage)


data HandshakeError
    = HandshakeTimeout
    | HandshakeConnError ConnectionError
    | HandshakeFailed    String
    deriving (Show, Eq)

data HandshakeState = HandshakeState
    { hsVersion   :: Maybe Version  -- 상대방의 version 메시지 객체
    , hsGotVerack :: Bool           -- 상대방의 verack 메시지 수신 여부
    } deriving (Show)

initialState :: HandshakeState
initialState = HandshakeState { hsVersion = Nothing, hsGotVerack = False }

-- | 핸드셰이크 타임아웃 10초 (마이크로초)
handshakeTimeoutUs :: Int
handshakeTimeoutUs = 10 * 1_000_000

performHandshake :: NodeConnection -> ExceptT HandshakeError IO Version
performHandshake conn = ExceptT $ do
    liftIO $ putStrLn "[Handshake] 양방향 핸드셰이크 시도"
    result <- timeout handshakeTimeoutUs $ runExceptT doHandshake
    case result of
        Nothing -> do
            putStrLn "[Handshake] 타임아웃: 응답 없음"
            return $ Left HandshakeTimeout
        Just r  ->
            return r
  where
    doHandshake :: ExceptT HandshakeError IO Version
    doHandshake = do
        sendVersion conn
        handshakeLoop conn initialState

handshakeLoop :: NodeConnection -> HandshakeState -> ExceptT HandshakeError IO Version
handshakeLoop _ (HandshakeState (Just peerVer) True) = do
    liftIO $ putStrLn "[Handshake] 양방향 핸드셰이크 완료"
    return peerVer
handshakeLoop conn handshakeSate = do
    msg <- liftConnectionException $ (recvMessage conn :: ExceptT ConnectionError IO Message)
    let cmd = BS8.unpack $ unCommand msg.header.command
    newHandshakeSate <- case cmd of
        "version" -> do
            case runGet get msg.payload of
                Left err ->
                    throwError $ HandshakeFailed $ "version 파싱 실패: " ++ err
                Right peerVer -> do
                    liftConnectionException $ sendMessage conn (Message (MessageHeader Mainnet (Command "verack")) BS.empty)
                    return handshakeSate { hsVersion = Just peerVer }
        "verack" -> do
            return handshakeSate { hsGotVerack = True }
        "ping" -> do
            liftIO $ putStrLn "[Handshake] 수신: ping -> 전송: pong"
            liftConnectionException $ sendMessage conn (Message (MessageHeader Mainnet (Command "pong")) msg.payload)
            return handshakeSate
        prematureCmd -> do
            liftIO $ putStrLn $ "[Handshake] 무시됨: " ++ prematureCmd
            return handshakeSate
    handshakeLoop conn newHandshakeSate

sendVersion :: NodeConnection -> ExceptT HandshakeError IO ()
sendVersion conn = do
    now   <- liftIO $ (round :: Double -> Word64) . realToFrac <$> getPOSIXTime
    nonce <- liftIO (randomIO :: IO Word64)
    let emptyAddr = NetworkAddress 0 (IPAddress (BS.replicate 16 0x00) 0)
        myVer     = Version
            { version     = 70015
            , services    = 0
            , timestamp   = now
            , addrRecv    = emptyAddr
            , addrFrom    = emptyAddr
            , nonce       = nonce
            , userAgent   = "/ProgrammingBitcoin:0.1/"
            , startHeight = 0
            , relay       = False
            }
    liftConnectionException $ sendMessage conn (Message (MessageHeader Mainnet (Command "version")) (buildVersionPayload myVer))

-- lift exception
liftConnectionException :: ExceptT ConnectionError IO a -> ExceptT HandshakeError IO a
liftConnectionException = ExceptT . fmap (either (Left . HandshakeConnError) Right) . runExceptT

liftHandshakeException :: ExceptT HandshakeError IO a -> ExceptT ConnectionError IO a
liftHandshakeException = ExceptT . fmap (either (Left . toConnError) Right) . runExceptT
  where
    toConnError HandshakeTimeout          = ConnectFailed "핸드셰이크 타임아웃"
    toConnError (HandshakeConnError e)    = e
    toConnError (HandshakeFailed reason)  = ConnectFailed ("핸드셰이크 실패: " ++ reason)

