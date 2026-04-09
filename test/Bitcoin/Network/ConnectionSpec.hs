module Bitcoin.Network.ConnectionSpec (spec) where

import qualified Test.Hspec as H

import Bitcoin.Network.Discovery (defaultDnsSeeds, getPeerIPs)
import Bitcoin.Network.Peer (runPeer)


spec :: H.Spec
spec = do
    H.describe "Connect Bitcoin Network" $ do
        H.it "test" $ do
            True `H.shouldBe` True
