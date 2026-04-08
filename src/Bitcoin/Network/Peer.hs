module Bitcoin.Network.Peer
    ( runPeer
    ) where

import Network.Socket                  (SockAddr)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Except            (ExceptT(..), runExceptT)
import Control.Monad.IO.Class          (liftIO)
import Data.Serialize                  (runGet, get)

import Bitcoin.Network.Message
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage)
import Bitcoin.Network.Handshake
    ( performHandshake, liftHandshakeException )


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
        putStrLn $ "  ├─ 프로토콜:   " ++ show peerVersion.versionVersion
        putStrLn $ "  ├─ 서비스:     " ++ show peerVersion.versionServices
        putStrLn $ "  ├─ 클라이언트: " ++ show peerVersion.versionUserAgent
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.versionStartHeight
    initialSyncState <- requestMessages conn
    handleMessageLoop conn initialSyncState 

data SyncState = SyncState
    { headersCompleted :: Bool
    , pingPong         :: Bool
    } deriving (Eq, Show)

-- 새 명령어를 추가할 때 이 함수만 수정하면 됩니다.
handleMessageLoop :: NodeConnection -> SyncState -> ExceptT ConnectionError IO ()
handleMessageLoop _ (SyncState True True) = do
    liftIO $ putStrLn "[Peer] 완료 상태 확인"
handleMessageLoop conn syncState = do
    msg :: Message <- recvMessage conn
    newSyncState   <- case BS8.unpack msg.header.command of
        "ping"         -> do
            liftIO $ putStrLn "[Peer] 수신: ping -> 전송: pong"
            sendMessage conn (buildMainnetMessage (Pong msg.payload))
            return syncState { pingPong = True }
        "headers"      -> do
            let payloadBS = msg.payload
            headersSyncState <- case runGet get msg.payload of
                Left err -> do
                    liftIO $ putStrLn $ "[Peer] 블록헤더 파싱 실패: " ++ err
                    return syncState
                Right (blockHeaders :: Headers) -> do
                    liftIO $ do
                        putStrLn $ "[Peer] 수신: headers (파싱 성공!)"
                        putStrLn $ "  ├─ Payload size:   " ++ show (BS.length msg.payload) ++ " bytes"
                        putStrLn $ "  └─ Headers count:  " ++ show (length blockHeaders.headersList)
                    return syncState { headersCompleted = True }
            liftIO $ do
                putStrLn $ "[Peer] 수신: headers"
                putStrLn $ "  ├─ Payload size (bytes):      " ++ show (BS.length payloadBS)
            return headersSyncState
        otherCmd       -> do
            liftIO $ putStrLn $ "[Peer] 수신 (무시됨): " ++ otherCmd
            return syncState 
    handleMessageLoop conn newSyncState 

requestMessages :: NodeConnection -> ExceptT ConnectionError IO SyncState
requestMessages conn = do
    let payload = GetHeaders
            { getHeadersVersion = 70015
            , getHeadersLocators = [genesisHashLE]
            , getHeadersHashStop = BS.replicate 32 0
            }
        msg = buildMainnetMessage payload
    sendMessage conn msg
    liftIO $ putStrLn "[Peer] 전송: getheaders (제네시스 블록 이후 2000개 요청)"
    return SyncState { headersCompleted = False, pingPong = False }

-- | 제네시스 블록 해시 (little-endian 직렬화)
-- block-heigth of 0
genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack $
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]

