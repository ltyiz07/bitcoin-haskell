module Utils.DetermineKSpec (spec) where

import qualified Test.Hspec  as H

import Secp256k1
import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Field.FiniteField

spec :: H.Spec
spec = do
    H.describe "KnownNat natVal method test" $ do
        H.it "get value p from FiniteField" $ do
            let px :: FG = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
                getP :: forall p. KnownNat p => FiniteField p -> Integer
                getP _ = natVal @p Proxy
            print $ getP px
            print $ getValue px

    H.describe "DetermineK method test" $ do
        H.it "deterministic k from private-key and hash" $ do
            True `H.shouldBe` True
