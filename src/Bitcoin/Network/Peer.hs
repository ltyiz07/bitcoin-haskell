module Bitcoin.Network.Peer
    ( runPeer
    , genesisHashLE
    ) where

import Network.Socket               (SockAddr)
import qualified Data.ByteString    as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Except         (ExceptT(..), runExceptT, throwError)
import Control.Monad.State          as ST
import Data.Serialize               as SR

import Bitcoin.Network.Message
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage )
import Bitcoin.Network.Handshake    ( performHandshake, liftHandshakeException )
import Utils.Hash                   ( hash256 )
import Utils.Hex                    ( bytesToHex )

-- ---------------------------------------------------------------------------
-- 타입
-- ---------------------------------------------------------------------------

-- | 동기화 단계와 단계별 데이터를 하나의 타입으로 표현합니다.
-- 각 생성자가 필요한 데이터만 들고 있으므로
-- SyncState 에 분산된 파생 필드(requestHeaders, isCompleted)가 불필요합니다.
data SyncPhase
    = Syncing BS.ByteString Int -- ^ (마지막으로 받은 블록 해시, 누적 수신 개수)
    | Completed Int             -- ^ (최종 수신 개수) — 모든 헤더 수신 완료
    deriving (Eq, Show)

-- | SyncPhase 외 변하지 않는 설정값만 남겼습니다.
data SyncState = SyncState
    { phase              :: SyncPhase
    , peerBlockHeight    :: Int
    , bhResBytesFilename :: String
    } deriving (Eq, Show)

type PeerM = ST.StateT SyncState (ExceptT ConnectionError IO)

-- ---------------------------------------------------------------------------
-- 진입점
-- ---------------------------------------------------------------------------

runPeer :: SockAddr -> IO ()
runPeer addr =
    runExceptT (withConnection addr peerSession) >>= \case
        Left  err -> putStrLn $ "[Peer] 종료: " ++ show err
        Right _   -> putStrLn   "[Peer] 정상 종료"

peerSession :: NodeConnection -> ExceptT ConnectionError IO ()
peerSession conn = do
    peerVersion <- liftHandshakeException $ performHandshake conn
    liftIO $ do
        putStrLn "[Peer] Peer version"
        putStrLn $ "  ├─ 프로토콜:   " ++ show peerVersion.versionVersion
        putStrLn $ "  ├─ 서비스:     " ++ show peerVersion.versionServices
        putStrLn $ "  ├─ 클라이언트: " ++ show peerVersion.versionUserAgent
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.versionStartHeight
    let initialState = SyncState
            { phase              = Syncing genesisHashLE 0
            , peerBlockHeight    = fromIntegral peerVersion.versionStartHeight
            , bhResBytesFilename = "data/blockheaders.dat"
            }
    ST.evalStateT (peerLoop conn) initialState

-- ---------------------------------------------------------------------------
-- 메인 루프
-- ---------------------------------------------------------------------------

-- | Phase 에 따라 요청을 보내거나 종료합니다.
-- [버그 3] 수정: getheaders 전송 후 dispatchLoop 로 진입하여
-- headers 응답이 올 때까지 ping 등 메시지를 처리합니다.
-- 이로써 ping 수신 후 불필요한 getheaders 중복 전송이 없어집니다.
peerLoop :: NodeConnection -> PeerM ()
peerLoop conn = do
    currentPhase <- ST.gets (.phase)
    case currentPhase of
        Completed total ->
            liftIO $ putStrLn $
                "[Peer] ✅ 동기화 완료 (총 " ++ show total ++ "개)"

        Syncing lastHash _ -> do
            ST.lift $ sendGetHeaders conn lastHash
            dispatchLoop conn

-- | headers 응답이 올 때까지 메시지를 처리합니다.
-- ping 등 유지 메시지는 처리 후 계속 대기하고,
-- headers 를 받으면 상태를 갱신한 뒤 peerLoop 로 복귀합니다.
dispatchLoop :: NodeConnection -> PeerM ()
dispatchLoop conn = do
    msg :: Message <- ST.lift $ recvMessage conn
    case BS8.unpack msg.header.command of
        "ping"    -> handlePing conn msg.payload >> dispatchLoop conn
        "headers" -> handleHeaders msg.payload   >> peerLoop conn
        otherCmd  -> do
            liftIO $ putStrLn $ "[Peer] 무시: " ++ otherCmd
            dispatchLoop conn

-- ---------------------------------------------------------------------------
-- 메시지 핸들러
-- ---------------------------------------------------------------------------

handlePing :: NodeConnection -> BS.ByteString -> PeerM ()
handlePing conn payload = do
    liftIO $ putStrLn "[Peer] ping → pong"
    ST.lift $ sendMessage conn (buildMainnetMessage (Pong payload))

-- | 헤더 수신 처리 및 Phase 전이.
-- [버그 4] 수정: newCount < 2000 과 totalCount >= peerBlockHeight 를
-- 함께 확인하여 마지막 배치가 정확히 2000개인 엣지케이스를 처리합니다.
handleHeaders :: BS.ByteString -> PeerM ()
handleHeaders payload = do
    blockHeaders :: Headers <- parseOrFail payload
    let newCount = length blockHeaders.headersList
    syncState <- ST.get

    case syncState.phase of
        Completed _ ->
            -- dispatchLoop 구조상 도달하지 않지만 방어 코드로 유지
            liftIO $ putStrLn "[Peer] 완료 상태에서 headers 수신 (무시)"

        Syncing _ oldCount -> do
            let totalCount = oldCount + newCount
                -- 두 조건 중 하나라도 참이면 체인 끝에 도달
                isChainEnd = newCount < 2000 || totalCount >= syncState.peerBlockHeight

            liftIO $ do
                putStrLn "[Peer] 수신: headers"
                putStrLn $ "  ├─ 수신 개수: " ++ show newCount
                putStrLn $ "  ├─ 누적 개수: " ++ show totalCount
                            ++ " / " ++ show syncState.peerBlockHeight
                putStrLn $ "  └─ 상태:      " ++
                    if isChainEnd then "체인 끝 도달 → Completed"
                                  else "계속 동기화 중"
                -- 첫 수신이면 새 파일, 이후는 이어 쓰기
                let serialized = runPut (SR.put blockHeaders)
                if oldCount == 0
                    then BS.writeFile  syncState.bhResBytesFilename serialized
                    else BS.appendFile syncState.bhResBytesFilename serialized

            ST.modify' $ \s -> s
                { phase = if isChainEnd
                            then Completed totalCount
                            else Syncing (blockHeaderHash (last blockHeaders.headersList)) totalCount
                }

-- ---------------------------------------------------------------------------
-- 유틸리티
-- ---------------------------------------------------------------------------

sendGetHeaders :: NodeConnection -> BS.ByteString -> ExceptT ConnectionError IO ()
sendGetHeaders conn fromHash = do
    let msg = buildMainnetMessage GetHeaders
                { getHeadersVersion  = 70015
                , getHeadersLocators = [fromHash]
                , getHeadersHashStop = BS.replicate 32 0
                }
    sendMessage conn msg
    liftIO $ putStrLn $ "[Peer] 전송: getheaders (from: " ++ bytesToHex fromHash ++ ")"

-- [버그 1] 수정: ST.lift 를 통해 ExceptT 레이어로 명시적으로 전달
parseOrFail :: SR.Serialize a => BS.ByteString -> PeerM a
parseOrFail bs = case SR.runGet SR.get bs of
    Left  err -> ST.lift $ throwError $ ParseError $ "파싱 실패: " ++ err
    Right val -> return val

-- | 블록 헤더의 해시를 계산합니다.
-- 비트코인 헤더는 80바이트이며, Headers 메시지의 BlockHeader 는
-- 80바이트 헤더 + tx count(1바이트)로 직렬화되므로 앞 80바이트만 해싱합니다.
blockHeaderHash :: BlockHeader -> BS.ByteString
blockHeaderHash = hash256 . BS.take 80 . runPut . SR.put

genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
