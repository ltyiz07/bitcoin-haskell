module Utils.PrimeSpec (spec) where

import qualified Test.Hspec  as H
import qualified Test.QuickCheck as QC
import Utils.Prime

spec :: H.Spec
spec = do
    H.describe "method: isPrime" $ do
        H.it "1 is not prime" $ do
            isPrime 1 `H.shouldBe` False
        H.it "2 is prime" $ do
            isPrime 2 `H.shouldBe` True
        H.it "prime numbers" $ do
            isPrime 3 `H.shouldBe` True
            isPrime 5 `H.shouldBe` True
            isPrime 7 `H.shouldBe` True
            isPrime 11 `H.shouldBe` True
            isPrime 13 `H.shouldBe` True
            isPrime 97 `H.shouldBe` True
            isPrime 101 `H.shouldBe` True
            isPrime 103 `H.shouldBe` True
            isPrime 1031 `H.shouldBe` True
        H.it "composite numbers" $ do
            -- Test FiniteField function call
            isPrime 4 `H.shouldBe` False
            isPrime 6 `H.shouldBe` False
            isPrime 8 `H.shouldBe` False
            isPrime 9 `H.shouldBe` False
            isPrime 10 `H.shouldBe` False
            isPrime 1023 `H.shouldBe` False
            isPrime 1024 `H.shouldBe` False
            isPrime 1025 `H.shouldBe` False
            isPrime 1026 `H.shouldBe` False
        H.it "numbers less than 0 is not prime numbers" $ do
            isPrime 0 `H.shouldBe` False
            isPrime (-1) `H.shouldBe` False
            isPrime (-2) `H.shouldBe` False
            isPrime (-3) `H.shouldBe` False

        H.describe "Propertise" $ do
            H.it "even numbers greater than 2 is not prime" $ QC.property $ \n ->
                let num = abs n * 2 + 4
                in not (isPrime num)
            H.it "performance test" $ QC.property $ \n ->
                let num = abs n * 2 + 10000
                in not (isPrime num)
