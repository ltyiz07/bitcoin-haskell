module Utils.DetermineK 
    ( determineK
    ) where

import GHC.TypeLits (KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Utils.Hash
import qualified Data.ByteString as B
import Data.Bits (shiftR)
import Secp256k1
import Numeric (showHex)

intToBytes32 :: Integer -> B.ByteString
intToBytes32 n = B.pack $ pad $ toBytes n
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x `mod` 256) : toBytes (x `shiftR` 8)
    pad bs    = replicate (32 - length bs) 0 ++ reverse bs

-- RFC 6979: 결정론적 k 생성
n :: Integer
n = secp256k1Order

determineK :: Integer -> Integer -> Integer
determineK e z = go k2 v2
  where
    z' = z `mod` n
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
        in if candidate >= 1 && candidate < n
           then candidate
           else
               -- 범위 밖이면 K, V 갱신 후 재시도
               let k' = hmac256 k (v' <> B.singleton 0x00)
                   v''= hmac256 k' v'
               in go k' v''

-- 빅엔디안 ByteString -> Integer
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

