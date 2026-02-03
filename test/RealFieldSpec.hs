module RealFieldSpec (spec) where

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import Field
import Field.RealField

spec :: H.Spec
spec = do
    H.describe "method: mkRealField" $ do
        H.it "construct RealField with valid values" $ do
            mkRealField 3.14 `H.shouldBe` RealField 3.14

    H.describe "Instance: RealField for Field TypeClass" $ do
        H.it "method: add" $ do
            let a = mkRealField 3.0
                b = mkRealField 5.0
                Just result = add a b
            getValue result `H.shouldBe` 8.0
        H.it "divide by zero test" $ do
            let a = mkRealField 5.0
                b = mkRealField 0.0
            divide a b `H.shouldBe` Nothing
