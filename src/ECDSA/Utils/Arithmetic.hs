{-# LANGUAGE BangPatterns #-}

module ECDSA.Utils.Arithmetic
    ( intToBytes32
    , bytesToInteger
    , gcdExt
    , invMod
    , powMod
    ) where

import qualified Data.ByteString as B
import Data.Bits (shiftR)


-- Integer -> 빅엔디안 32 ByteString
intToBytes32 :: Integer -> B.ByteString
intToBytes32 n = B.pack $ pad $ toBytes n
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x `mod` 256) : toBytes (x `shiftR` 8)
    pad bs    = replicate (32 - length bs) 0 ++ reverse bs

-- 빅엔디안 ByteString -> Integer
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | 확장 유클리드 알고리즘: ax + by = gcd(a, b)를 만족하는 (g, x, y) 반환
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt 0 b = (b, 0, 1)
gcdExt a b = 
    let (g, x1, y1) = gcdExt (b `rem` a) a
        x = y1 - (b `quot` a) * x1
        y = x1
    in (g, x, y)

-- | 모듈로 역원: a * x ≡ 1 (mod m) 을 만족하는 x 반환
invMod :: Integer -> Integer -> Integer
invMod a m = 
    let (g, x, _) = gcdExt a m
    in if g /= 1 
       then error "Arithmetic Error: Modular inverse does not exist (not coprime)"
       else x `mod` m

-- | 모듈로 거듭제곱: base^exp (mod m) 계산 (음수 지수 지원)
powMod :: Integer -> Integer -> Integer -> Integer
powMod b e m
    | m == 1    = 0
    | e < 0     = powMod (invMod b m) (abs e) m
    | otherwise = go (b `mod` m) e 1
  where
    go _ 0 !acc = acc
    go base expo !acc
        | odd expo   = go (base * base `mod` m) (expo `quot` 2) (acc * base `mod` m)
        | otherwise = go (base * base `mod` m) (expo `quot` 2) acc
