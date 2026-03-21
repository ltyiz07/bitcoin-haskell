{-# LANGUAGE BangPatterns #-}

module EllipticCurve
    ( Point(..)
    , EllipticCurve(..)
    , mkEllipticCurve
    , mkPointOnCurve
    , isOnCurve
    , addPoint
    , multiplyPoint
    , negatePoint
    ) where

import Data.Ratio () -- Needed for Integer literals to be interpreted as Rational for RationalField

data Point f
    = Point
        { x :: f
        , y :: f
        }
    | Infinity
    deriving (Show, Eq)

-- y^2 = x^3 + a*x + b
data EllipticCurve f = EllipticCurve
    { a :: f
    , b :: f
    } deriving (Show, Eq)

mkEllipticCurve :: (Fractional f, Eq f) => f -> f -> EllipticCurve f
mkEllipticCurve a b = EllipticCurve a b

mkPointOnCurve :: (Fractional f, Eq f) => EllipticCurve f -> f -> f -> Maybe (Point f)
mkPointOnCurve curve x y
    | isOnCurve curve point = Just point
    | otherwise             = Nothing
  where point = Point x y -- Direct construction now, assuming x, y are valid field elements

isOnCurve :: (Fractional f, Eq f) => EllipticCurve f -> Point f -> Bool
isOnCurve _ Infinity = True
isOnCurve (EllipticCurve a b) (Point x y) =
    y^(2::Integer) == x^(3::Integer) + a*x + b

addPoint :: (Fractional f, Eq f) => EllipticCurve f -> Point f -> Point f -> Point f
addPoint _ Infinity point2 = point2
addPoint _ point1 Infinity = point1
addPoint (EllipticCurve a _b) (Point x1 y1) (Point x2 y2)
    -- Case: point is its own negative (y=0, vertical tangent)
    | x1 == x2 && y1 == 0 = Infinity
    -- Case: same points (tangent)
    | x1 == x2 && y1 == y2 =
        let
            s = (3 * x1^(2::Integer) + a) / (2 * y1)
            x3 = s^(2::Integer) - 2 * x1
            y3 = s * (x1 - x3) - y1
        in Point x3 y3
    -- Case: same x, different y (inverse points)
    | x1 == x2 = Infinity
    -- Case: different points
    | otherwise =
        let
            s = (y2 - y1) / (x2 - x1)
            x3 = s^(2::Integer) - x1 - x2
            y3 = s * (x1 - x3) - y1
        in Point x3 y3

multiplyPoint :: (Fractional f, Eq f) => EllipticCurve f -> Integer -> Point f -> Point f
multiplyPoint curve k p
    | k == 0 = Infinity
    | k < 0 = multiplyPoint curve (-k) (negatePoint p)
    | otherwise = go k p Infinity
  where
    go !n !cur !acc
        | n == 0    = acc
        | odd n     = go nextN nextCur (addPoint curve acc cur)
        | otherwise = go nextN nextCur acc
      where
        nextCur = addPoint curve cur cur
        nextN = quot n 2

negatePoint :: (Num f) => Point f -> Point f
negatePoint Infinity = Infinity
negatePoint (Point x y) = Point x (negate y)
