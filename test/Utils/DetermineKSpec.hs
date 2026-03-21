module Utils.DetermineKSpec (spec) where

import qualified Test.Hspec  as H

spec :: H.Spec
spec = do
    H.describe "DetermineK method test" $ do
        H.it "deterministic k from private-key and hash" $ do
            True `H.shouldBe` True
