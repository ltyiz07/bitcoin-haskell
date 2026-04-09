module Bitcoin.Network.Peer
    ( runPeer
    , genesisHashLE
    ) where

import Network.Socket                  (SockAddr)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Except            (ExceptT(..), runExceptT, throwError)
import Control.Monad.State             as ST
import Data.Serialize                  as SR

import Bitcoin.Network.Message
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage)
import Bitcoin.Network.Handshake
    ( performHandshake, liftHandshakeException )
import Utils.Hash ( hash256 )


type PeerM = ST.StateT SyncState (ExceptT ConnectionError IO)

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
        putStrLn $ "  ├─ recv:       " ++ show peerVersion.versionAddrRecv
        putStrLn $ "  ├─ frmo:       " ++ show peerVersion.versionAddrFrom
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.versionStartHeight
    
    sendGetHeaders conn genesisHashLE 
    -- SyncState initial
    let initialSyncState = SyncState
            { lastHeaderHash = genesisHashLE 
            , peerBlockHeight = fromIntegral peerVersion.versionStartHeight
            , currentHeaderCount = 0
            , bhResBytesFilename = "data/blockheaders_20260409_01.dat"
            }
    ST.evalStateT (peerLoop conn) initialSyncState

data SyncState = SyncState
    { lastHeaderHash     :: BS.ByteString
    , peerBlockHeight    :: Int
    , currentHeaderCount :: Int
    , bhResBytesFilename :: String
    } deriving (Eq, Show)

peerLoop :: NodeConnection -> PeerM ()
peerLoop conn = do
    -- SyncState completed (finish)
    isCompleted <- ST.gets (\s -> s.peerBlockHeight == s.currentHeaderCount)
    syncState <- ST.get
    liftIO $ print syncState 
    if isCompleted 
        then do
            liftIO $ putStrLn "[Peer] 완료 상태 확인 (모든 작업 완료)"
        else do
            msg :: Message <- ST.lift $ recvMessage conn
            case msg.header.command of
                "ping"    -> handlePing conn msg.payload
                "headers" -> handleHeaders conn msg.payload
                otherCmd  -> liftIO $ putStrLn $ "[Peer] 수신 (무시됨): " ++ BS8.unpack otherCmd
            peerLoop conn

-- -------------------------------------------------------------------------
-- 메시지 핸들러 (상태 조작 로직 분리)
-- -------------------------------------------------------------------------

handlePing :: NodeConnection -> BS.ByteString -> PeerM ()
handlePing conn payload = do
    liftIO $ putStrLn "[Peer] 수신: ping -> 전송: pong"
    ST.lift $ sendMessage conn (buildMainnetMessage (Pong payload))

handleHeaders :: NodeConnection -> BS.ByteString -> PeerM ()
handleHeaders conn payload = do
    blockHeaders :: Headers <- parseOrFail payload
    liftIO $ do
        putStrLn $ "[Peer] 수신: headers (파싱 성공!)"
        putStrLn $ "  ├─ Payload size:   " ++ show (BS.length payload) ++ " bytes"
        putStrLn $ "  └─ Headers count:  " ++ show (length blockHeaders.headersList)
    syncState <- ST.get
    if syncState.currentHeaderCount == 0
        then liftIO $ BS.writeFile syncState.bhResBytesFilename (runPut . SR.put $ blockHeaders)
        else liftIO $ BS.appendFile syncState.bhResBytesFilename (runPut . SR.put $ blockHeaders)
    let recvedLastHeaderHash = getBlockHeaderHash (last blockHeaders.headersList)
    ST.modify' (\s -> s
        { currentHeaderCount = s.currentHeaderCount + length blockHeaders.headersList
        , lastHeaderHash = recvedLastHeaderHash
        })
    ST.lift $ sendGetHeaders conn recvedLastHeaderHash
  where
    getBlockHeaderHash :: BlockHeader -> BS.ByteString
    getBlockHeaderHash = hash256 . BS.take 80 . runPut . SR.put

-- -------------------------------------------------------------------------
-- 유틸리티 함수
-- -------------------------------------------------------------------------
parseOrFail :: Serialize a => BS.ByteString -> PeerM a
parseOrFail payload = case SR.runGet SR.get payload of
    Left err -> 
        throwError $ ParseError $ "메세지 파싱 실패: " ++ err
    Right parsedData -> 
        return parsedData

sendGetHeaders :: NodeConnection -> BS.ByteString -> ExceptT ConnectionError IO ()
sendGetHeaders conn lastHeaderHash = do
    let payload = GetHeaders
            { getHeadersVersion = 70015
            , getHeadersLocators = [lastHeaderHash]
            , getHeadersHashStop = BS.replicate 32 0
            }
        msg = buildMainnetMessage payload
    sendMessage conn msg
    liftIO $ putStrLn "[Peer] 전송: getheaders (제네시스 블록 이후 2000개 요청)"

-- | 제네시스 블록 해시 (little-endian 직렬화)
genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack $
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
