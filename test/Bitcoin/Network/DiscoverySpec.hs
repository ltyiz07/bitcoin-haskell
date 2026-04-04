module Bitcoin.Network.DiscoverySpec (spec) where

import qualified Test.Hspec as H

import Bitcoin.Network.Discovery


spec :: H.Spec
spec = do
    H.describe "Connect Bitcoin Network" $ do
        H.it "test" $ do
            let (seedDomain, seedPort) = head defaultDnsSeeds 
            ips <- getPeerIPs "google.com" "443" 
            print ips
            length ips `H.shouldSatisfy` (> 0)
