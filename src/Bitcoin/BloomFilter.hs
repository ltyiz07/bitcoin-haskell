module Bitcoin.BloomFilter
    ( BloomFilter(..)
    , newBloomFilter
    , bloomFilterAdd
    , bloomFilterContains
    , murmurHash3
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, shiftR, xor, (.|.), (.&.))

-- | Bloom Filter structure as defined in BIP 37.
data BloomFilter = BloomFilter
    { bfData      :: BS.ByteString -- ^ The filter content (bit array)
    , bfHashFuncs :: Word32        -- ^ Number of hash functions
    , bfTweak     :: Word32        -- ^ Random tweak to randomize the hash
    , bfFlags     :: Word8         -- ^ Matching flags (how to update the filter)
    } deriving (Show, Eq)

-- | Create a new Bloom Filter with all zeros.
newBloomFilter :: Int -> Word32 -> Word32 -> Word8 -> BloomFilter
newBloomFilter size hashFuncs tweak flags =
    BloomFilter
        { bfData = BS.replicate size 0
        , bfHashFuncs = hashFuncs
        , bfTweak = tweak
        , bfFlags = flags
        }

-- | Add an element to the Bloom Filter.
bloomFilterAdd :: BloomFilter -> BS.ByteString -> BloomFilter
bloomFilterAdd bf item =
    let bitCount = fromIntegral (BS.length bf.bfData * 8)
        indices = [ murmurHash3 (i * 0xfba4c795 + bf.bfTweak) item `mod` bitCount
                  | i <- [0 .. bf.bfHashFuncs - 1]
                  ]
        newData = foldl setBit bf.bfData indices
    in bf { bfData = newData }

-- | Check if an element might be in the Bloom Filter.
bloomFilterContains :: BloomFilter -> BS.ByteString -> Bool
bloomFilterContains bf item =
    let bitCount = fromIntegral (BS.length bf.bfData * 8)
        indices = [ murmurHash3 (i * 0xfba4c795 + bf.bfTweak) item `mod` bitCount
                  | i <- [0 .. bf.bfHashFuncs - 1]
                  ]
    in all (isBitSet bf.bfData) indices

-- ---------------------------------------------------------------------------
-- 비트 조작 유틸리티
-- ---------------------------------------------------------------------------

setBit :: BS.ByteString -> Word32 -> BS.ByteString
setBit bs idx =
    let byteIdx = fromIntegral (idx `div` 8)
        bitIdx  = fromIntegral (idx `mod` 8)
        byte    = BS.index bs byteIdx
        newByte = byte .|. (1 `shiftL` bitIdx)
        (before, after) = BS.splitAt byteIdx bs
    in before `BS.append` (BS.singleton newByte `BS.append` BS.tail after)

isBitSet :: BS.ByteString -> Word32 -> Bool
isBitSet bs idx =
    let byteIdx = fromIntegral (idx `div` 8)
        bitIdx  = fromIntegral (idx `mod` 8)
        byte    = BS.index bs byteIdx
    in (byte .&. (1 `shiftL` bitIdx)) /= 0

-- ---------------------------------------------------------------------------
-- MurmurHash3 (32-bit) 구현
-- ---------------------------------------------------------------------------

murmurHash3 :: Word32 -> BS.ByteString -> Word32
murmurHash3 seed bs =
    let c1 = 0xcc9e2d51
        c2 = 0x1b873593
        r1 = 15
        r2 = 13
        m  = 5
        n  = 0xe6546b64

        len = BS.length bs
        numBlocks = len `div` 4
        
        -- 처리 루프 (4바이트 블록 단위)
        h1 = foldl (\h i ->
            let k1 = getBlock i
                k1' = k1 * c1
                k1'' = (k1' `shiftL` r1) .|. (k1' `shiftR` (32 - r1))
                k1''' = k1'' * c2
                h1' = h `xor` k1'''
                h1'' = (h1' `shiftL` r2) .|. (h1' `shiftR` (32 - r2))
            in h1'' * m + n
            ) seed [0 .. numBlocks - 1]

        -- 남은 바이트 처리 (Tail)
        tailStart = numBlocks * 4
        remaining = len `mod` 4
        k1_tail = getTail tailStart remaining
        
        h1_final = if remaining > 0
            then h1 `xor` (((k1_tail * c1) `shiftL` r1) .|. ((k1_tail * c1) `shiftR` (32 - r1))) * c2
            else h1

        -- 마무리 (Finalization)
        h1_len = h1_final `xor` fromIntegral len
        h1_mix1 = (h1_len `xor` (h1_len `shiftR` 16)) * 0x85ebca6b
        h1_mix2 = (h1_mix1 `xor` (h1_mix1 `shiftR` 13)) * 0xc2b2ae35
    in h1_mix2 `xor` (h1_mix2 `shiftR` 16)

  where
    getBlock i =
        let off = i * 4
        in fromIntegral (BS.index bs off)
            .|. (fromIntegral (BS.index bs (off + 1)) `shiftL` 8)
            .|. (fromIntegral (BS.index bs (off + 2)) `shiftL` 16)
            .|. (fromIntegral (BS.index bs (off + 3)) `shiftL` 24)

    getTail off remLen =
        let b1 = if remLen >= 1 then fromIntegral (BS.index bs off) else 0
            b2 = if remLen >= 2 then fromIntegral (BS.index bs (off + 1)) `shiftL` 8 else 0
            b3 = if remLen >= 3 then fromIntegral (BS.index bs (off + 2)) `shiftL` 16 else 0
        in b1 .|. b2 .|. b3
