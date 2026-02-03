module Field.RealField
    ( RealField(..)
    , mkRealField
    ) where

import Field

-- | 실수체 원소 타입
newtype RealField = RealField
    { getValue :: Double
    } deriving (Show, Eq)

-- | RealField에 대한 Field 인스턴스
instance Field RealField where
    add (RealField a) (RealField b) = Just $ RealField (a + b)
    
    sub (RealField a) (RealField b) = Just $ RealField (a - b)
    
    mult (RealField a) (RealField b) = Just $ RealField (a * b)
    
    divide (RealField a) (RealField b)
        | b == 0    = Nothing
        | otherwise = Just $ RealField (a / b)
    
    pow (RealField a) n = RealField (a ^ n)
    
    inv (RealField a)
        | a == 0    = Nothing
        | otherwise = Just $ RealField (1 / a)

-- | 실수체 원소 생성
mkRealField :: Double -> RealField
mkRealField = RealField
