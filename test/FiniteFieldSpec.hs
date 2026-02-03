module FiniteFieldSpec (spec) where

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import Field
import Field.FiniteField

spec :: H.Spec
spec = do
    H.describe "method: mkFiniteField" $ do
        H.it "construct FiniteField with valid values" $ do
            mkFiniteField 3 7 `H.shouldBe` Just (FiniteField 3 7)
            mkFiniteField 4 11 `H.shouldBe` Just (FiniteField 4 11)
        
        H.it "deny minus values" $ do
            mkFiniteField (-1) 7 `H.shouldBe` Nothing
            mkFiniteField (-2) 7 `H.shouldBe` Nothing

    H.describe "Instance: FiniteField for Field TypeClass" $ do
        H.it "method: add" $ do
            let a = FiniteField 3 7
                b = FiniteField 5 7
            add a b `H.shouldBe` Just (FiniteField 1 7)
        
        H.it "method: mult" $ do
            let a = FiniteField 3 7
                b = FiniteField 5 7
            mult a b `H.shouldBe` Just (FiniteField 1 7)
        
        H.it "method: pow" $ do
            let a = FiniteField 2 7
            pow a 3 `H.shouldBe` FiniteField 1 7
        
        H.it "method:: inv" $ do
            let a = FiniteField 3 7
                Just invVal = inv a
                Just result = mult a invVal
            result `H.shouldBe` FiniteField 1 7

