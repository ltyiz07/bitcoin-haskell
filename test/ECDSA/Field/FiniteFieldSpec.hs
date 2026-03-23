module ECDSA.Field.FiniteFieldSpec (spec) where

import qualified Test.Hspec as H
import Control.Exception (evaluate)
import ECDSA.Field.FiniteField

spec :: H.Spec
spec = do
    H.describe "Num instance for FiniteField" $ do
        H.it "constructs from integer" $ do
            let a :: FiniteField 7 = 3
            a `H.shouldBe` FiniteField 3
            let b :: FiniteField 7 = fromInteger 10
            b `H.shouldBe` FiniteField 3
        H.it "adds two finite fields" $ do
            let a :: FiniteField 7 = 3
                b :: FiniteField 7 = 5
            a + b `H.shouldBe` FiniteField 1
        H.it "multiplies two finite fields" $ do
            let a :: FiniteField 7 = 3
                b :: FiniteField 7 = 5
            a * b `H.shouldBe` FiniteField 1
        H.it "calculates powers" $ do
            let a :: FiniteField 7 = 2
            a ^ (3::Integer) `H.shouldBe` FiniteField 1
            let b :: FiniteField 13 = 3
            b ^ (3::Integer) `H.shouldBe` FiniteField 1

    H.describe "Fractional instance for FiniteField" $ do
        H.it "calculates reciprocal (inverse)" $ do
            let a :: FiniteField 7 = 3
            recip a `H.shouldBe` FiniteField 5 -- 3 * 5 = 15 `mod` 7 = 1
            let b :: FiniteField 13 = 4
            recip b `H.shouldBe` FiniteField 10 -- 4 * 10 = 40 `mod` 13 = 1
        H.it "divides two finite fields" $ do
            let a :: FiniteField 19 = 2
                b :: FiniteField 19 = 7
            -- 2 / 7 mod 19 = 2 * (recip 7) mod 19
            -- recip 7 mod 19 = 11 (since 7 * 11 = 77 = 4*19 + 1)
            -- 2 * 11 = 22 mod 19 = 3
            a / b `H.shouldBe` FiniteField 3
        H.it "throws an error when dividing by zero" $ do
            let a :: FiniteField 13 = 5
                b :: FiniteField 13 = 0
            evaluate (a / b) `H.shouldThrow` H.anyErrorCall
