module Utils.Base58
    ( encodeBase58
    , decodeBase58
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import Utils.Arithmetic (bytesToInteger, intToBytes)


b58Alphabet :: B.ByteString
b58Alphabet = C.pack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

intToBase58 :: Integer -> B.ByteString
intToBase58 0 = B.empty
intToBase58 n = B.reverse $ B.unfoldr step n
  where
    step 0 = Nothing
    step x = let (q, r) = x `quotRem` 58
             in Just (B.index b58Alphabet (fromIntegral r), q)

base58ToInt :: B.ByteString -> Either String Integer
base58ToInt = B.foldl' step (Right 0)
  where
    step (Left e) _ = Left e
    step (Right acc) w =
        case B.elemIndex w b58Alphabet of
            Just val -> Right (acc * 58 + fromIntegral val)
            Nothing  -> Left $ "Invalid Base58 character: " ++ [toEnum (fromIntegral w)]

encodeBase58 :: B.ByteString -> B.ByteString
encodeBase58 bs =
    let (zeroBytes, restBytes) = B.span (== 0x00) bs
        zeroCount = B.length zeroBytes
        prefix = B.replicate zeroCount (B.index b58Alphabet 0)
        encodedRest = if B.null restBytes 
                      then B.empty 
                      else intToBase58 (bytesToInteger restBytes)
    in prefix <> encodedRest

decodeBase58 :: B.ByteString -> Either String B.ByteString
decodeBase58 b58 = do
    let (ones, restB58) = B.span (== 0x31) b58
        zeroCount = B.length ones
        prefix = B.replicate zeroCount 0x00
    if B.null restB58
        then return prefix
        else do
            intVal <- base58ToInt restB58
            let decodedRest = intToBytes intVal
            return $ prefix <> decodedRest
