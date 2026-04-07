{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.Peer
    ( runPeer
    ) where

import Network.Socket                  (SockAddr)
import Data.Serialize                  (runPut, putWord32le, putWord8, putByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Except            (ExceptT(..), runExceptT)
import Control.Monad.IO.Class          (liftIO)

import Bitcoin.Network.Message.Payload.Version
import Bitcoin.Network.Message.Header
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage)
import Bitcoin.Network.Handshake       (performHandshake, liftHandshakeException)


runPeer :: SockAddr -> IO ()
runPeer addr = do
    result <- runExceptT $ withConnection addr peerSession
    case result of
        Left err -> putStrLn $ "[Peer] 종료: " ++ show err
        Right _  -> putStrLn "[Peer] 정상 종료"

peerSession :: NodeConnection -> ExceptT ConnectionError IO ()
peerSession conn = do
    peerVersion <- liftHandshakeException $ performHandshake conn
    liftIO $ do
        putStrLn $ "[Peer] Peer version"
        putStrLn $ "  ├─ 프로토콜:   " ++ show peerVersion.version
        putStrLn $ "  ├─ 서비스:     " ++ show peerVersion.services
        putStrLn $ "  ├─ 클라이언트: " ++ BS8.unpack peerVersion.userAgent
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.startHeight
    initialSyncState <- sendGetHeaders conn
    handleMessageLoop conn initialSyncState 

-- data SyncState = SyncState
newtype SyncState = SyncState
    { headersCompleted :: Bool
    } deriving (Eq, Show)

-- 새 명령어를 추가할 때 이 함수만 수정하면 됩니다.
handleMessageLoop :: NodeConnection -> SyncState -> ExceptT ConnectionError IO ()
handleMessageLoop _ (SyncState True) = do
    liftIO $ putStrLn "[Peer] 완료 상태 확인"
handleMessageLoop conn syncState = do
    msg <- (recvMessage conn :: ExceptT ConnectionError IO Message)
    let cmd = BS8.unpack $ unCommand msg.header.command
    newSyncState <- case cmd of
        "ping" -> do
            liftIO $ putStrLn "[Peer] 수신: ping -> 전송: pong"
            sendMessage conn (Message (MessageHeader Mainnet (Command "pong")) mempty)
            return syncState 
        "headers" -> do
            let payload = msg.payload
            liftIO $ do
                putStrLn $ "[Peer] 📦 블록 헤더 수신! (" ++ show (BS.length payload) ++ " bytes)"
            return syncState { headersCompleted = True }
        otherCmd -> do
            liftIO $ putStrLn $ "[Peer] 수신 (무시됨): " ++ otherCmd
            return syncState 
    handleMessageLoop conn newSyncState 

sendGetHeaders :: NodeConnection -> ExceptT ConnectionError IO SyncState
sendGetHeaders conn = do
    let payload = runPut $ do
                putWord32le 70015
                putWord8 1                          -- Locator 해시 개수
                putByteString genesisHashLE         -- 시작 해시 (little-endian)
                putByteString (BS.replicate 32 0)   -- 종료 해시 (0 = 끝까지)
        msg = Message (MessageHeader Mainnet (Command "getheaders")) payload
    sendMessage conn msg
    liftIO $ putStrLn "[Peer] 🚀 전송: getheaders (제네시스 블록 이후 2000개 요청)"
    return SyncState { headersCompleted = False }

-- | 제네시스 블록 해시 (little-endian 직렬화)
genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]


