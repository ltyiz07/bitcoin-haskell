module Bitcoin.Keys.SEC
    ( SECFormat(..)
    , encodeSEC
    , decodeSEC
    ) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import GHC.TypeLits
import Data.Proxy

import ECDSA.Curve.EllipticCurve (Point(..), mkPointOnCurve)
import ECDSA.Curve.Secp256k1 (FG, secp256k1)
import ECDSA.Field.FiniteField (FiniteField(..))
import ECDSA.Utils.Arithmetic (intToBytes32, bytesToInteger, powMod)


data SECFormat = Compressed | Uncompressed
    deriving (Show, Eq)

encodeSEC :: SECFormat -> Point FG -> B.ByteString
encodeSEC Compressed   (Point x y) = encodeSECCompressed x y
encodeSEC Uncompressed (Point x y) = encodeSECUncompressed x y
encodeSEC _            Infinity    = error "Cannot encode Infinity point in SEC format"

decodeSEC :: B.ByteString -> Maybe (Point FG)
decodeSEC bs =
    case B.uncons bs of
        Nothing -> Nothing
        Just (prefix, xyBytes) ->
            case prefix of
                0x02 -> decodeSECCompressed False xyBytes -- Even case
                0x03 -> decodeSECCompressed True xyBytes
                0x04 -> decodeSECUncompressed xyBytes
                _    -> Nothing

encodeSECCompressed :: FiniteField p -> FiniteField p -> B.ByteString
encodeSECCompressed x y =
    let prefix = if even (getValue y) then 0x02 else 0x03
    in B.cons prefix (intToBytes32 (getValue x))

encodeSECUncompressed :: FiniteField p -> FiniteField p -> B.ByteString
encodeSECUncompressed x y = B.concat [B.singleton 0x04, intToBytes32 (getValue x), intToBytes32 (getValue y)]

decodeSECCompressed :: Bool -> B.ByteString -> Maybe (Point FG)
decodeSECCompressed isOdd xBytes
    | B.length xBytes /= 32 = Nothing
    | otherwise =
        let x = fromInteger $ bytesToInteger xBytes :: FG
            ySquare = x ^ 3 + 7
        in case sqrtModP ySquare of
            Nothing -> Nothing
            Just (evenY, oddY) -> 
                let y = if isOdd then oddY else evenY
                in mkPointOnCurve secp256k1 x y

decodeSECUncompressed :: B.ByteString -> Maybe (Point FG)
decodeSECUncompressed xyBytes
    | B.length xyBytes /= 64 = Nothing
    | otherwise =
        let (xBytes, yBytes) = B.splitAt 32 xyBytes
            x = fromInteger $ bytesToInteger xBytes
            y = fromInteger $ bytesToInteger yBytes
        in mkPointOnCurve secp256k1 x y

-- Return: (Even-Y, Odd-Y)
sqrtModP :: forall p. KnownNat p => FiniteField p -> Maybe (FiniteField p, FiniteField p)
sqrtModP n =
    let p_val     = natVal (Proxy @p)
        expo  = (p_val + 1) `quot` 4
        candidate = fromInteger $ powMod (getValue n) expo p_val
    in if p_val `mod` 4 /= 3
       then Nothing
       else if candidate * candidate == n
            then
                let evenY = if even (getValue candidate) then candidate else negate candidate
                    oddY  = negate evenY
                in Just (evenY, oddY)
            else Nothing
