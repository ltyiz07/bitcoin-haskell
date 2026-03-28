module ECDSA.Signature
    ( Signature(..)
    , verify
    , sign
    , derivePublicPoint
    , determineK
    ) where

import qualified Data.ByteString as B

import ECDSA.Curve.Secp256k1
import ECDSA.Curve.EllipticCurve
import ECDSA.Utils.Arithmetic
import ECDSA.Field.FiniteField
import ECDSA.Utils.Hash


{-
 -   - r: x value of Random point (R === kG)
 -   - s: Signature
 -   - z: Message hash (of Hash-256)
 -   - e: Private key value
 -   - publicPoint: Public key
 -}

data Signature = Signature
    { r :: Integer
    , s :: Integer
    } deriving (Show, Eq)

verify :: Point FG -> Signature -> Integer -> Bool
verify publicPoint (Signature r s) z
    | r <= 0 || r >= secp256k1Order = False
    | s <= 0 || s >= secp256k1Order = False
    | s > (secp256k1Order `quot` 2) = False
    | otherwise = 
        let invS = invMod s secp256k1Order
            u = (z * invS) `mod` secp256k1Order
            v = (r * invS) `mod` secp256k1Order
            uG = multiplyPoint secp256k1 u gPoint
            vP = multiplyPoint secp256k1 v publicPoint
            addedPoint = addPoint secp256k1 uG vP
        in case addedPoint of
            Infinity   -> False
            Point rx _ -> getValue rx == r

-- | RFC 6979와 BIP146(Low-S)을 준수하는 서명 함수
sign :: Integer -> Integer -> Signature
sign e z = captureValidSig (determineK e z)
  where
    n     = secp256k1Order
    halfN = n `quot` 2
    captureValidSig []     = error "sign: no valid k found - should be impossible!"
    captureValidSig (k:ks) =
        let rPoint = multiplyPoint secp256k1 k gPoint
        in case rPoint of
            Infinity   -> captureValidSig ks
            Point rx _ ->
                let rVal = getValue rx `mod` n
                    sRaw = (z + rVal * e) * invMod k n `mod` n
                    sVal = if sRaw > halfN then n - sRaw else sRaw
                in if rVal == 0 || sVal == 0
                    then captureValidSig ks
                    else Signature rVal sVal

-- | RFC 6979: 결정론적 k 후보들을 무한히 생성하는 리스트 (Lazy List)
determineK :: Integer -> Integer -> [Integer]
determineK e z = go k2 v2
  where
    z' = z `mod` secp256k1Order
    bz = intToBytes32 z'
    be = intToBytes32 e
    v0 = B.replicate 32 0x01
    k0 = B.replicate 32 0x00
    k1 = hmac256 k0 (v0 <> B.singleton 0x00 <> be <> bz)
    v1 = hmac256 k1 v0
    k2 = hmac256 k1 (v1 <> B.singleton 0x01 <> be <> bz)
    v2 = hmac256 k2 v1
    go k v =
        let v'        = hmac256 k v
            candidate = bytesToInteger v'
            kNext     = hmac256 k     (v' <> B.singleton 0x00)
            vNext     = hmac256 kNext v'
        in if candidate >= 1 && candidate < secp256k1Order
           then candidate : go kNext vNext
           else go kNext vNext

derivePublicPoint :: Integer -> Point FG
derivePublicPoint e = multiplyPoint secp256k1 e gPoint
