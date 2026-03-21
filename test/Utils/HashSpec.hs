module Utils.HashSpec (spec) where

import qualified Test.Hspec  as H
import Utils.Hash

spec :: H.Spec
spec = do
    H.describe "Hash method test" $ do
        H.it "Hash-256 test" $ do
            hash256 "This is it!" `H.shouldBe` "3d67c04d1cc090e5370ce848ee4907abde1381964cd6fbd2e601a00a0c98656a"
