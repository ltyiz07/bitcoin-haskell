module ECDSA.Signature
    ( Signature(..)
    , verify
    , sign
    , derivePublicPoint
    , determineK
    , intToBytes32
    , bytesToInteger
    ) where

import qualified Data.ByteString as B
import Data.Bits (shiftR)

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
    | otherwise = 
        let invS = invMod s secp256k1Order
            u = (z * invS) `mod` secp256k1Order
            v = (r * invS) `mod` secp256k1Order
            uG = multiplyPoint secp256k1 u gPoint
            vP = multiplyPoint secp256k1 v publicPoint
            addedPoint = addPoint secp256k1 uG vP
        in case addedPoint of
            Infinity -> False
            Point rx _ -> getValue rx == r

sign :: Integer -> Integer -> Signature
sign e z = Signature (modN r) (modN s)
  where
    modN = flip mod secp256k1Order
    k = determineK e z
    r = getValue $ x (multiplyPoint secp256k1 k gPoint)
    s = (z + r * e) * (invMod k secp256k1Order)

derivePublicPoint :: Integer -> Point FG
derivePublicPoint e = multiplyPoint secp256k1 e gPoint

determineK :: Integer -> Integer -> Integer
determineK e z = go k2 v2
  where
    z' = z `mod` secp256k1Order
    bz  = intToBytes32 z'
    be = intToBytes32 e

    -- Step 1: 초기화
    v0 = B.replicate 32 0x01
    k0 = B.replicate 32 0x00

    -- Step 2~5: K, V 초기 설정
    k1 = hmac256 k0 (v0 <> B.singleton 0x00 <> be <> bz)
    v1 = hmac256 k1 v0
    k2 = hmac256 k1 (v1 <> B.singleton 0x01 <> be <> bz)
    v2 = hmac256 k2 v1

    -- Step 6: 후보 k 생성 및 검증
    go k v =
        let v'        = hmac256 k v
            candidate = bytesToInteger v'
        in if candidate >= 1 && candidate < secp256k1Order
           then candidate
           else
               -- 범위 밖이면 K, V 갱신 후 재시도
               let k' = hmac256 k (v' <> B.singleton 0x00)
                   v''= hmac256 k' v'
               in go k' v''

intToBytes32 :: Integer -> B.ByteString
intToBytes32 n = B.pack $ pad $ toBytes n
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x `mod` 256) : toBytes (x `shiftR` 8)
    pad bs    = replicate (32 - length bs) 0 ++ reverse bs

-- 빅엔디안 ByteString -> Integer
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

