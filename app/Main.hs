{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Network.Socket (SockAddr)

import Bitcoin.Network.Peer (runPeer, PeerConfig(..), PeerTask(..))
import Bitcoin.Network.PeerSelection (findBestPeers, PeerMetrics(..))
import Bitcoin.Network.Discovery (defaultDnsSeeds, getPeerIPs)
import Utils.Hex (hexToBytes)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["headers", filename] -> 
            startPeer (PeerConfig TaskSyncHeaders filename)
        
        ["block", blockHashHex, filename] -> 
            let blockHash = BS.reverse $ hexToBytes blockHashHex
            in startPeer (PeerConfig (TaskDownloadBlocks [blockHash]) filename)
        
        ["verify", txIdHex, blockHashHex, filename] ->
            let txId = BS.reverse $ hexToBytes txIdHex
                blockHash = BS.reverse $ hexToBytes blockHashHex
            in startPeer (PeerConfig (TaskVerifyTx txId [blockHash]) filename)
        
        _ -> printUsage

startPeer :: PeerConfig -> IO ()
startPeer config = do
    let (seedDomain, seedPort) = head defaultDnsSeeds
    ips <- getPeerIPs seedDomain seedPort
    if null ips
        then putStrLn "[Main] 연결 가능한 피어를 찾을 수 없습니다."
        else do
            -- 최소 높이 940,000 이상의 피어 중 가장 좋은 피어 1개 선택
            bestPeers <- findBestPeers ips 940000 1
            case bestPeers of
                [] -> putStrLn "[Main] 조건을 만족하는(높이 940k+) 피어가 없습니다."
                (best:_) -> do
                    putStrLn $ "[Main] 최적의 피어 선택됨: " ++ show (best.pmAddr)
                             ++ " (높이: " ++ show (best.pmHeight)
                             ++ ", 지연: " ++ show (round (best.pmLatency * 1000) :: Int) ++ "ms)"
                    runPeer (best.pmAddr) config

printUsage :: IO ()
printUsage = do
    putStrLn "사용법 (Usage):"
    putStrLn "  cabal run impl-btc -- headers <파일명>"
    putStrLn "  cabal run impl-btc -- block <블록해시_HEX> <파일명>"
    putStrLn "  cabal run impl-btc -- verify <트랜잭션ID_HEX> <블록해시_HEX> <파일명>"
    putStrLn ""
    putStrLn "예시 (Example):"
    putStrLn "  cabal run impl-btc -- verify 635070903332766336e974e4604e28e4e7e682e0789823f668700938e3e9d89c 00000000000000000001710e4f1b98e7bd77f1d5ca4874cf7b869403f10c9d1f data/verify.dat"
