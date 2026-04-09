module Main (main) where

import Bitcoin.Network.Peer

import Bitcoin.Network.Discovery (defaultDnsSeeds, getPeerIPs)
import Bitcoin.Network.Peer (runPeer)


main :: IO ()
main = do
    let (seedDomain, seedPort) = head defaultDnsSeeds
    ips <- getPeerIPs seedDomain seedPort
    if null ips
        then putStrLn "연결가능한 네트워크 탐색 실패"
        else do
            let firstIp = head ips
            runPeer firstIp
