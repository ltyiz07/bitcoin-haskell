module Bitcoin.Network.Verifier
    ( verifyHeadersIntegrity
    , calculateNewTarget
    , epochRangeFromHeight
    , targetToBits
    , bitsToTarget
    , getMerkleRoot 
    , getWitnessmerkleRoot
    ) where

import Data.Word                    (Word32)
import Data.Bits                    (shiftR, shiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.Serialize  as SR
import Bitcoin.Network.Message
import Utils.Hash (hash256)

-- Returns: Hash of last block header if chain of headers are valid
verifyHeadersIntegrity :: BS.ByteString -> [BlockHeader] -> Maybe BS.ByteString
verifyHeadersIntegrity initialHash headers = foldl' foldlFunc (Just initialHash) headers
  where
    foldlFunc :: Maybe BS.ByteString -> BlockHeader -> Maybe BS.ByteString
    foldlFunc Nothing     _                                   = Nothing
    foldlFunc (Just hash) header | hash == header.bhPrevBlock = Just (getHeaderHash header)
                                 | otherwise                  = Nothing
    getHeaderHash :: BlockHeader -> BS.ByteString
    getHeaderHash = hash256 . BS.take 80 . SR.runPut . SR.put

-- 2016번째 블록과 1번째 블록의 타임스탬프를 사용하여 계산합니다.
calculateNewTarget :: Integer -> Word32 -> Word32 -> Integer
calculateNewTarget oldTarget firstBlockTimestamp lastBlockTimestamp =
    let 
        expectedTime :: Integer
        expectedTime = 1209600 -- 14일 (2016 * 10 * 60 초)
        actualTimeRaw :: Integer
        actualTimeRaw = fromIntegral lastBlockTimestamp - fromIntegral firstBlockTimestamp
        actualTime :: Integer
        actualTime
            | actualTimeRaw < expectedTime `div` 4 = expectedTime `div` 4
            | actualTimeRaw > expectedTime * 4     = expectedTime * 4
            | otherwise                            = actualTimeRaw
        newTarget :: Integer
        newTarget = (oldTarget * actualTime) `div` expectedTime
        maxTarget :: Integer
        maxTarget = bitsToTarget 0x1d00ffff
    in 
        min newTarget maxTarget

-- Returns (First block height in epoch, Last block height in epoch)
epochRangeFromHeight :: Integral a => a -> (a, a)
epochRangeFromHeight blockHeight = let lastHeight = blockHeight `div` 2016 * 2016 - 1 in (lastHeight - 2015, lastHeight)

targetToBits :: Integer -> Word32
targetToBits 0 = 0
targetToBits target =
    let 
        bytes = bytesNeeded target
        shifted = if bytes <= 3
                  then target `shiftL` (8 * (3 - bytes))
                  else target `shiftR` (8 * (bytes - 3))
        coef = shifted .&. 0xffffffff
        (finalCoef, finalBytes) = if coef .&. 0x00800000 /= 0
                                  then (coef `shiftR` 8, bytes + 1)
                                  else (coef, bytes)
        expo = fromIntegral finalBytes `shiftL` 24
    in 
        fromIntegral (expo .|. finalCoef)
  where
    bytesNeeded :: Integer -> Int
    bytesNeeded 0 = 0
    bytesNeeded n = 1 + bytesNeeded (n `shiftR` 8)

bitsToTarget :: Word32 -> Integer
bitsToTarget bits =
    let 
        expo = fromIntegral $ bits `shiftR` 24
        coef = fromIntegral $ bits .&. 0x007fffff
        isNegative = (bits .&. 0x00800000) /= 0
        target = if expo <= 3
                 then coef `shiftR` (8 * (3 - expo))
                 else coef `shiftL` (8 * (expo - 3))
    in 
        if isNegative then -target else target

getMerkleRoot :: [Tx] -> Hash32
getMerkleRoot [] = error "Empty transaction list"
getMerkleRoot txs = head $ toRoot (fmap txId txs)

getWitnessmerkleRoot :: [Tx] -> Hash32
getWitnessmerkleRoot [] = error "Empty transaction list"
getWitnessmerkleRoot txs = head $ toRoot (fmap wtxId txs)

toRoot :: [Hash32] -> [Hash32]
toRoot []          = []
toRoot (root : []) = [root]
toRoot internal    = toRoot (getUpperLayer internal)
  where
    getUpperLayer :: [Hash32] -> [Hash32]
    getUpperLayer []             = []
    getUpperLayer (l : [])       = getUpperLayer [l, l]
    getUpperLayer (l : r : rest) = Hash32 (hash256 (unHash32 l <> unHash32 r)) : getUpperLayer rest

