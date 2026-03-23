{-# LANGUAGE BangPatterns #-}

module ECDSA.Utils.Arithmetic
    ( gcdExt
    , invMod
    , powMod
    ) where

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
    go base exp !acc
        | odd exp   = go (base * base `mod` m) (exp `quot` 2) (acc * base `mod` m)
        | otherwise = go (base * base `mod` m) (exp `quot` 2) acc
