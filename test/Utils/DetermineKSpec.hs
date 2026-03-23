module Utils.DetermineKSpec (spec) where

import qualified Test.Hspec  as H

import ECDSA.Utils.DetermineK

spec :: H.Spec
spec = do
    H.describe "DetermineK method test" $ do
        H.it "deterministic k from private-key and hash" $ do
            let e = 0x2bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b
                z = 0xe96cf59c385c8903c6c795d3ce27a49730df2ce0133c830271abac6b5352beb6
                result = determineK e z
                expected = 0x2580e99c46234781424f08b8f6e9b3d4616c27fd3e3cc559bdb52b85dc2bacb5
            result `H.shouldBe` expected
