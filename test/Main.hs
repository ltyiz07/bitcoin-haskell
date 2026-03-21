module Main (main) where

import Test.Hspec
import qualified PrimeSpec
import qualified FiniteFieldSpec
import qualified RationalFieldSpec
import qualified EllipticCurveSpec
import qualified Secp256k1Spec

main :: IO ()
main = hspec $ do
    PrimeSpec.spec
    FiniteFieldSpec.spec
    RationalFieldSpec.spec
    EllipticCurveSpec.spec
    Secp256k1Spec.spec
