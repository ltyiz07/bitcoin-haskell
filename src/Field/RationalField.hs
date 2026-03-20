{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Field.RationalField
    ( RationalField(..)
    , mkRationalField
    ) where

import Data.Ratio ()

-- | 유리수체 원소 타입
newtype RationalField = RationalField
    { getValue :: Rational
    } deriving (Show, Eq, Num, Fractional) -- Num and Fractional derived from Rational

-- | 유리수체 원소 생성 (from Integer)
mkRationalField :: Integer -> RationalField
mkRationalField = fromInteger