module Main (main) where

import Test.Hspec

import qualified ECDSA.Utils.PrimeSpec
import qualified ECDSA.Utils.HashSpec
import qualified ECDSA.Field.FiniteFieldSpec
import qualified ECDSA.Field.RationalFieldSpec
import qualified ECDSA.Curve.EllipticCurveSpec
import qualified ECDSA.SignatureSpec
import qualified Bitcoin.Keys.SECSpec
import qualified Bitcoin.Keys.DERSpec


main :: IO ()
main = hspec $ do
    ECDSA.Utils.PrimeSpec.spec
    ECDSA.Utils.HashSpec.spec
    ECDSA.Field.FiniteFieldSpec.spec
    ECDSA.Field.RationalFieldSpec.spec
    ECDSA.Curve.EllipticCurveSpec.spec
    ECDSA.SignatureSpec.spec
    Bitcoin.Keys.SECSpec.spec
    Bitcoin.Keys.DERSpec.spec
