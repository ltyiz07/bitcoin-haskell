module ECDSA.Field.RationalFieldSpec (spec) where

import qualified Test.Hspec as H
import Control.Exception (evaluate)
import Data.Ratio ((%))
import ECDSA.Field.RationalField

spec :: H.Spec
spec = do
    H.describe "Num instance for RationalField" $ do
        H.it "constructs from integer" $ do
            mkRationalField 3 `H.shouldBe` RationalField (3 % 1)
        H.it "adds two rational fields" $ do
            let a = RationalField (1 % 2)
                b = RationalField (1 % 3)
            a + b `H.shouldBe` RationalField (5 % 6)
        H.it "multiplies two rational fields" $ do
            let a = RationalField (2 % 3)
                b = RationalField (3 % 4)
            a * b `H.shouldBe` RationalField (6 % 12) -- which is 1/2
            a * b `H.shouldBe` RationalField (1 % 2)  -- Test simplification

    H.describe "Fractional instance for RationalField" $ do
        H.it "divides two rational fields" $ do
            let a = RationalField (1 % 2)
                b = RationalField (1 % 4)
            a / b `H.shouldBe` RationalField (2 % 1)
        H.it "throws an error when dividing by zero" $ do
            let a = RationalField (5 % 1)
                b = RationalField (0 % 1)
            evaluate (a / b) `H.shouldThrow` H.anyArithException
