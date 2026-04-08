module Bitcoin.Network.Peer
    ( runPeer
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

data SyncState = SyncState
    { headersCompleted :: Bool
    , pingPong         :: Bool
    } deriving (Eq, Show)


-- StateT와 ExceptT를 결합한 커스텀 모나드 타입 정의
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
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.versionStartHeight
    
    sendGetHeaders conn
    let initialSyncState = SyncState False False
    -- StateT 환경 실행 (초기 상태 주입)
    ST.evalStateT (peerLoop conn) initialSyncState

-- 루프 자체에서는 상태를 명시적으로 인자로 받지 않음
peerLoop :: NodeConnection -> PeerM ()
peerLoop conn = do
    currentState <- ST.get
    if currentState == SyncState True True
        then do
            liftIO $ putStrLn "[Peer] 완료 상태 확인 (모든 작업 완료)"
    -- ExceptT 계층의 네트워크 수신 연산을 lift로 끌어옴
        else do
            msg :: Message <- ST.lift $ recvMessage conn
            case msg.header.command of
                "ping"    -> handlePing conn msg.payload
                "headers" -> handleHeaders msg.payload
                otherCmd  -> liftIO $ putStrLn $ "[Peer] 수신 (무시됨): " ++ BS8.unpack otherCmd
            -- 상태가 업데이트된 채로 다음 루프 실행
            peerLoop conn

-- -------------------------------------------------------------------------
-- 메시지 핸들러 (상태 조작 로직 분리)
-- -------------------------------------------------------------------------

handlePing :: NodeConnection -> BS.ByteString -> PeerM ()
handlePing conn payload = do
    liftIO $ putStrLn "[Peer] 수신: ping -> 전송: pong"
    ST.lift $ sendMessage conn (buildMainnetMessage (Pong payload))
    -- pingPong 상태를 True로 덮어쓰기
    ST.modify' (\s -> s { pingPong = True })

handleHeaders :: BS.ByteString -> PeerM ()
handleHeaders payload = do
    blockHeaders :: Headers <- parseOrFail payload
    liftIO $ do
        putStrLn $ "[Peer] 수신: headers (파싱 성공!)"
        putStrLn $ "  ├─ Payload size:   " ++ show (BS.length payload) ++ " bytes"
        putStrLn $ "  └─ Headers count:  " ++ show (length blockHeaders.headersList)
    -- headersCompleted 상태를 True로 덮어쓰기
    ST.modify' (\s -> s { headersCompleted = True })

-- -------------------------------------------------------------------------
-- 유틸리티 함수
-- -------------------------------------------------------------------------
parseOrFail :: Serialize a => BS.ByteString -> PeerM a
parseOrFail payload = case SR.runGet SR.get payload of
    Left err -> 
        throwError $ ParseError $ "메세지 파싱 실패: " ++ err
    Right parsedData -> 
        return parsedData

sendGetHeaders :: NodeConnection -> ExceptT ConnectionError IO ()
sendGetHeaders conn = do
    let payload = GetHeaders
            { getHeadersVersion = 70015
            , getHeadersLocators = [genesisHashLE]
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
