module Bitcoin.Network.ConnectionSpec (spec) where

import qualified Test.Hspec as H

import Bitcoin.Network.Discovery (defaultDnsSeeds, getPeerIPs)
import Bitcoin.Network.Peer (runPeer)


spec :: H.Spec
spec = do
    H.describe "Connect Bitcoin Network" $ do
        H.it "test" $ do
            let (seedDomain, seedPort) = head defaultDnsSeeds
            ips <- getPeerIPs seedDomain seedPort
            if null ips
                then putStrLn "연결 가능한 노드가 없습니다."
                else do
                    let firstIp = head ips
                    runPeer firstIp

