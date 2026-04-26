module Bitcoin.Network.PeerSelection
    ( PeerMetrics(..)
    , findBestPeers
    , measurePeer
    ) where

import Network.Socket               (SockAddr)
import Data.Time.Clock              (getCurrentTime, diffUTCTime)
import Control.Monad.Except         (runExceptT)
import Data.List                    (sortOn)
import Data.Maybe                   (mapMaybe)
import Control.Concurrent.Async     (mapConcurrently)
import Control.Exception            (try, SomeException)
import Data.Word                     (Word32)

import Bitcoin.Network.Connection   (withConnection)
import Bitcoin.Network.Handshake    (performHandshake, liftHandshakeException)
import Bitcoin.Network.Message       (Version(..))

-- | 피어의 성능 지표
data PeerMetrics = PeerMetrics
    { pmAddr    :: SockAddr
    , pmHeight  :: Int
    , pmLatency :: Double -- 초 단위 (밀리초 정밀도)
    } deriving (Show, Eq)

-- | 여러 피어를 동시에 측정하여 가장 좋은 피어들을 반환
findBestPeers :: [SockAddr] -> Int -> Int -> IO [PeerMetrics]
findBestPeers addrs minHeight limit = do
    putStrLn $ "[Selector] " ++ show (length addrs) ++ "개의 피어 성능 측정 시작..."
    -- 병렬로 피어들 측정
    metrics <- mapConcurrently measurePeer (take 20 addrs) -- 너무 많으면 리소스 과부하되므로 20개씩 끊어서 시도
    
    let validPeers = filter (\p -> p.pmHeight >= minHeight) (mapMaybe id metrics)
        -- 높이가 높은 순, 그 다음 응답 속도가 빠른 순으로 정렬
        sorted = sortOn (\p -> (-p.pmHeight, p.pmLatency)) validPeers
    
    return $ take limit sorted

-- | 단일 피어의 핸드셰이크 속도와 높이 측정
measurePeer :: SockAddr -> IO (Maybe PeerMetrics)
measurePeer addr = do
    start <- getCurrentTime
    res <- try @SomeException $ runExceptT $ withConnection addr $ \conn -> do
        peerVer <- liftHandshakeException $ performHandshake conn
        let h = peerVer.versionStartHeight :: Word32
        return (fromIntegral h :: Int)
    
    end <- getCurrentTime
    let latency = realToFrac $ diffUTCTime end start
    
    case res of
        Right (Right h) -> do
            putStrLn $ "[Selector] 피어 응답: " ++ show addr ++ " (높이: " ++ show h ++ ", 지연: " ++ show (round (latency * 1000) :: Int) ++ "ms)"
            return $ Just (PeerMetrics addr h latency)
        _ -> return Nothing
