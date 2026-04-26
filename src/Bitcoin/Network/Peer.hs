module Bitcoin.Network.Peer
    ( runPeer
    , genesisHashLE
    , PeerConfig(..)
    , PeerTask(..)
    ) where

import Network.Socket               (SockAddr)
import qualified Data.ByteString    as BS
import Control.Monad                (when, forM_)
import Control.Monad.Except         (ExceptT(..), runExceptT, throwError)
import Control.Monad.State          as ST
import Data.Serialize               as SR

import Bitcoin.Network.Message
import Bitcoin.BloomFilter
import Bitcoin.MerkleTree
import Bitcoin.Network.Connection
    ( NodeConnection, ConnectionError(..), withConnection, sendMessage, recvMessage )
import Bitcoin.Network.Handshake    ( performHandshake, liftHandshakeException )
import Utils.Hash                   ( hash256 )
import Utils.Hex                    ( bytesToHex, hexToBytes )

-- ---------------------------------------------------------------------------
-- 타입
-- ---------------------------------------------------------------------------

data PeerTask
    = TaskSyncHeaders
    | TaskDownloadBlocks [BS.ByteString]
    | TaskVerifyTx BS.ByteString [BS.ByteString] -- Target TxId, Block hashes to check
    deriving (Eq, Show)

data PeerConfig = PeerConfig
    { task         :: PeerTask
    , saveFilename :: FilePath
    } deriving (Eq, Show)

data SyncPhase
    = SyncHeaders BS.ByteString Int
    | SyncBlocks [BS.ByteString] Int
    | Completed Int
    | Listening
    deriving (Eq, Show)

data SyncState = SyncState
    { phase              :: SyncPhase
    , peerBlockHeight    :: Int
    , resFilename        :: String
    , targetTxIds        :: [BS.ByteString]
    } deriving (Eq, Show)

type PeerM = ST.StateT SyncState (ExceptT ConnectionError IO)

-- ---------------------------------------------------------------------------
-- 진입점
-- ---------------------------------------------------------------------------

runPeer :: SockAddr -> PeerConfig -> IO ()
runPeer addr config =
    runExceptT (withConnection addr (peerSession config)) >>= \case
        Left  err -> putStrLn $ "[Peer] 종료: " ++ show err
        Right _   -> putStrLn   "[Peer] 정상 종료"

peerSession :: PeerConfig -> NodeConnection -> ExceptT ConnectionError IO ()
peerSession config conn = do
    peerVersion <- liftHandshakeException $ performHandshake conn
    liftIO $ do
        putStrLn "[Peer] 연결 성공"
        putStrLn $ "  ├─ 프로토콜:   " ++ show peerVersion.versionVersion
        putStrLn $ "  └─ 블록 높이:  " ++ show peerVersion.versionStartHeight
    
    -- Task 에 따른 초기화 로직
    case config.task of
        TaskSyncHeaders -> do
            let initialState = SyncState
                    { phase           = SyncHeaders genesisHashLE 0
                    , peerBlockHeight = fromIntegral peerVersion.versionStartHeight
                    , resFilename     = config.saveFilename
                    , targetTxIds     = []
                    }
            ST.evalStateT (peerLoop conn) initialState

        TaskDownloadBlocks hashes -> do
            let initialState = SyncState
                    { phase           = SyncBlocks hashes 0
                    , peerBlockHeight = fromIntegral peerVersion.versionStartHeight
                    , resFilename     = config.saveFilename
                    , targetTxIds     = []
                    }
            ST.evalStateT (peerLoop conn) initialState

        TaskVerifyTx targetTx blockHashes -> do
            -- 블룸 필터 등록
            let emptyFilter = newBloomFilter 10 3 0 1
                myFilter = bloomFilterAdd emptyFilter targetTx
            sendFilterLoad conn myFilter
            
            let initialState = SyncState
                    { phase           = SyncBlocks blockHashes 0
                    , peerBlockHeight = fromIntegral peerVersion.versionStartHeight
                    , resFilename     = config.saveFilename
                    , targetTxIds     = [targetTx]
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

        SyncHeaders lastHash _ -> do
            ST.lift $ sendGetHeaders conn lastHash
            dispatchLoop conn

        SyncBlocks [] downloadedCount -> do
            ST.modify' $ \s -> s { phase = Completed downloadedCount }
            peerLoop conn
        SyncBlocks (targetHash : _) _ -> do
            syncState <- ST.get
            -- VerifyTx 작업인 경우에만 filtered data(merkleblock) 요청
            case syncState.targetTxIds of
                [] -> ST.lift $ sendGetData conn targetHash
                _  -> ST.lift $ sendGetFilteredData conn targetHash
            dispatchLoop conn

        Listening -> do
            liftIO $ putStrLn "[Peer] 🎧 리스닝 모드 진입 (네트워크 메시지 대기 중...)"
            dispatchLoop conn

-- | headers 응답이 올 때까지 메시지를 처리합니다.
-- ping 등 유지 메시지는 처리 후 계속 대기하고,
-- headers 를 받으면 상태를 갱신한 뒤 peerLoop 로 복귀합니다.
dispatchLoop :: NodeConnection -> PeerM ()
dispatchLoop conn = do
    msg :: Message <- ST.lift $ recvMessage conn
    case msg.header.command of
        "ping"        -> handlePing conn msg.payload >> dispatchLoop conn
        "headers"     -> handleHeaders msg.payload   >> peerLoop conn
        "block"       -> handleBlock msg.payload     >> peerLoop conn
        "merkleblock" -> handleMerkleBlock conn msg.payload >> peerLoop conn
        "inv"         -> handleInv conn msg.payload  >> dispatchLoop conn
        otherCmd      -> do
            liftIO $ putStrLn $ "[Peer] 무시: " ++ show otherCmd
            dispatchLoop conn

-- ---------------------------------------------------------------------------
-- 메시지 핸들러
-- ---------------------------------------------------------------------------

handleMerkleBlock :: NodeConnection -> BS.ByteString -> PeerM ()
handleMerkleBlock _conn payload = do
    mb :: MerkleBlock <- parseOrFail payload
    let pmt = PartialMerkleTree mb.mbTotalTxs mb.mbHashes mb.mbFlags
        currentBlockHash = blockHeaderHash mb.mbHeader
    
    liftIO $ do
        putStrLn $ "[Peer] 수신: merkleblock (" ++ bytesToHex currentBlockHash ++ ")"
        putStrLn $ "  ├─ 해시 개수: " ++ show (length mb.mbHashes)
        putStrLn $ "  └─ 총 트랜잭션: " ++ show mb.mbTotalTxs
    
    case extractMatches pmt of
        Left err -> liftIO $ putStrLn $ "  ⚠️ 머클 블록 검증 실패: " ++ err
        Right (recalcRoot, matched) -> do
            syncState <- ST.get
            let targets = syncState.targetTxIds
            liftIO $ do
                -- 파일 저장
                if syncState.phase == SyncBlocks [currentBlockHash] 0 -- 첫 블록이면 새로 쓰기
                    then BS.writeFile syncState.resFilename payload
                    else BS.appendFile syncState.resFilename payload

                let expectedRoot = mb.mbHeader.bhMerkleRoot
                putStrLn $ "  ├─ 계산된 루트: " ++ bytesToHex recalcRoot
                putStrLn $ "  ├─ 헤더 루트:   " ++ bytesToHex expectedRoot
                if recalcRoot == expectedRoot
                    then putStrLn "  ✅ 머클 루트 일치!"
                    else putStrLn "  ❌ 머클 루트 불일치 (검증 실패)!"
                
                putStrLn $ "  └─ 매칭된 트랜잭션 (" ++ show (length matched) ++ "개):"
                forM_ matched $ \txid -> do
                    let isTarget = txid `elem` targets
                    putStrLn $ "     - " ++ bytesToHex txid ++ (if isTarget then " ⭐ [검증됨: 목표 트랜잭션]" else "")

            -- SyncBlocks 페이즈인 경우 다음 블록으로 진행
            case syncState.phase of
                SyncBlocks (expectedHash : rest) downloadedCount 
                    | currentBlockHash == expectedHash -> do
                        liftIO $ putStrLn "  └─ 동기화 진행 중... 다음 블록 요청 예정"
                        ST.modify' $ \s -> s { phase = SyncBlocks rest (downloadedCount + 1) }
                _ -> return ()

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
        SyncHeaders _ oldCount -> do
            let totalCount = oldCount + newCount
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
                    then BS.writeFile  syncState.resFilename serialized
                    else BS.appendFile syncState.resFilename serialized

            ST.modify' $ \s -> s
                { phase = if isChainEnd
                            then Completed totalCount
                            else SyncHeaders (blockHeaderHash (last blockHeaders.headersList)) totalCount
                }
        _ -> liftIO $ putStrLn "[Peer] 예상치 못한 메시지 수신 -- Unreachable"


handleBlock :: BS.ByteString -> PeerM ()
handleBlock payload = do
    block :: Block <- parseOrFail payload
    syncState <- ST.get
    
    case syncState.phase of
        SyncBlocks (currentHash : rest) downloadedCount -> do
            liftIO $ do
                putStrLn $ "[Peer] 수신: block (" ++ bytesToHex currentHash ++ ")"
                putStrLn $ "  ├─ 트랜잭션 수: " ++ show (length block.blockTxs)
                putStrLn $ "  └─ 크기:        " ++ show (BS.length payload) ++ " bytes"
            
            if downloadedCount == 0
                then liftIO $ BS.writeFile syncState.resFilename payload
                else liftIO $ BS.appendFile syncState.resFilename payload
            
            ST.modify' $ \s -> s { phase = SyncBlocks rest (downloadedCount + 1) }
            
        _ -> liftIO $ putStrLn "[Peer] 예상치 못한 메시지 수신 -- Unreachable"

handleInv :: NodeConnection -> BS.ByteString -> PeerM ()
handleInv conn payload = do
    invMsg :: Inv <- parseOrFail payload
    let items = invMsg.inventory
        count = length items
    
    liftIO $ do
        putStrLn "[Peer] 수신: inv"
        putStrLn $ "  ├─ 아이템 개수: " ++ show count
    
    case items of
        [] -> return ()
        (firstItem:_) -> do
            liftIO $ putStrLn $ "  └─ 첫 번째 항목: " ++ show firstItem.invType ++ " - " ++ bytesToHex firstItem.invHash
            
            -- 블록 광고인 경우 자동으로 merkleblock 요청
            forM_ items $ \iv -> do
                case iv.invType of
                    MsgBlock -> ST.lift $ sendGetFilteredData conn iv.invHash
                    MsgWitnessBlock -> ST.lift $ sendGetFilteredData conn iv.invHash
                    _ -> return ()

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

sendGetData :: NodeConnection -> BS.ByteString -> ExceptT ConnectionError IO ()
sendGetData conn targetHash = do
    let inv = InvVector { invType = MsgWitnessBlock, invHash = targetHash }
        msg = buildMainnetMessage (GetData [inv])
    sendMessage conn msg
    liftIO $ putStrLn $ "[Peer] 전송: getdata (block: " ++ bytesToHex targetHash ++ ")"

sendFilterLoad :: NodeConnection -> BloomFilter -> ExceptT ConnectionError IO ()
sendFilterLoad conn bf = do
    let payload = FilterLoad
            { flFilter     = bf.bfData
            , flHashFuncs  = bf.bfHashFuncs
            , flTweak      = bf.bfTweak
            , flFlags      = bf.bfFlags
            }
        msg = buildMainnetMessage payload
    sendMessage conn msg
    liftIO $ putStrLn "[Peer] 전송: filterload"

sendGetFilteredData :: NodeConnection -> BS.ByteString -> ExceptT ConnectionError IO ()
sendGetFilteredData conn targetHash = do
    let inv = InvVector { invType = MsgFilteredBlock, invHash = targetHash }
        msg = buildMainnetMessage (GetData [inv])
    sendMessage conn msg
    liftIO $ putStrLn $ "[Peer] 전송: getdata (filtered block: " ++ bytesToHex targetHash ++ ")"

parseOrFail :: SR.Serialize a => BS.ByteString -> PeerM a
parseOrFail bs = case SR.runGet SR.get bs of
    Left  err -> ST.lift $ throwError $ ParseError $ "파싱 실패: " ++ err
    Right val -> return val

blockHeaderHash :: BlockHeader -> BS.ByteString
blockHeaderHash = hash256 . BS.take 80 . runPut . SR.put

genesisHashLE :: BS.ByteString
genesisHashLE = BS.pack
    [ 0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72
    , 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f
    , 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c
    , 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00
    ]

-- TODO: implement method properly
getTargetBlockHashes :: Integer -> Integer -> [BS.ByteString]
getTargetBlockHashes _from _to = map (BS.reverse . hexToBytes)
    [ "00000000000000000000df0f80044130bb616de91fb0a1d61f27cdcf20458233" -- 930000
    , "00000000000000000001d8d7d2a2f0cbb5b5fc689486df2c49532cfee8942604" -- 930001
    , "00000000000000000000078b3ac20f1bf14ec4512bedf71ef41365e168967a14" -- 930002
    , "000000000000000000017627bb4c4b6de783f24a39621093878d50f5fb75a080" -- 930003
    , "00000000000000000001710e4f1b98e7bd77f1d5ca4874cf7b869403f10c9d1f" -- 930004
    ]
