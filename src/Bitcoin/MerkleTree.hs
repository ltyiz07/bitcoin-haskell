module Bitcoin.MerkleTree
    ( PartialMerkleTree(..)
    , extractMatches
    , calcTreeHeight
    , calcTreeNodes
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Data.Bits (testBit)
import Utils.Hash (hash256)

-- | Partial Merkle Tree structure used in 'merkleblock' messages.
data PartialMerkleTree = PartialMerkleTree
    { pmtTotalTxs :: Word32          -- ^ Total number of transactions in the block
    , pmtHashes   :: [BS.ByteString] -- ^ List of hashes (txid or internal nodes)
    , pmtFlags    :: BS.ByteString   -- ^ Bits indicating tree traversal
    } deriving (Show, Eq)

-- | Result of extracting matches from a Partial Merkle Tree.
data ExtractionResult = ExtractionResult
    { erRoot          :: BS.ByteString   -- ^ Reconstructed Merkle Root
    , erMatchedHashes :: [BS.ByteString] -- ^ List of matched transaction hashes
    } deriving (Show, Eq)

-- | Reconstruct the Merkle Root and extract matched transaction hashes.
-- Returns (MerkleRoot, MatchedTxHashes).
extractMatches :: PartialMerkleTree -> Either String (BS.ByteString, [BS.ByteString])
extractMatches pmt =
    let height = calcTreeHeight pmt.pmtTotalTxs
        initialState = (0, 0, pmt.pmtHashes, []) -- (flagIdx, hashIdx, remainingHashes, matched)
    in case traverseTree height 0 pmt initialState of
        Left err -> Left err
        Right (root, (_, _, [], matched)) -> Right (root, reverse matched)
        Right (_, (_, _, leftHashes, _)) -> Left $ "남은 해시가 있습니다: " ++ show (length leftHashes)

-- | Recursive tree traversal as defined in BIP 37.
traverseTree :: Int -> Word32 -> PartialMerkleTree -> (Int, Int, [BS.ByteString], [BS.ByteString])
             -> Either String (BS.ByteString, (Int, Int, [BS.ByteString], [BS.ByteString]))
traverseTree height pos pmt (fIdx, hIdx, hashes, matched) =
    let flag = testBit (BS.index pmt.pmtFlags (fIdx `div` 8)) (fIdx `mod` 8)
    in if height == 0 || not flag
        then -- Leaf node or pre-computed internal node
            case hashes of
                (h:hs) -> 
                    let newMatched = if height == 0 && flag then h : matched else matched
                    in Right (h, (fIdx + 1, hIdx + 1, hs, newMatched))
                [] -> Left "해시가 부족합니다."
        else -- Internal node that needs descending
            let leftPos = pos * 2
                rightPos = pos * 2 + 1
                totalAtLevel = calcTreeNodes (fromIntegral pmt.pmtTotalTxs) (height - 1)
            in do
                -- Left child
                (leftRoot, s1) <- traverseTree (height - 1) leftPos pmt (fIdx + 1, hIdx, hashes, matched)
                -- Right child (if exists, otherwise use left child's hash as per Bitcoin Merkle tree rules)
                if rightPos < totalAtLevel
                    then do
                        (rightRoot, s2) <- traverseTree (height - 1) rightPos pmt s1
                        return (hash256 (leftRoot `BS.append` rightRoot), s2)
                    else
                        return (hash256 (leftRoot `BS.append` leftRoot), s1)

-- ---------------------------------------------------------------------------
-- 유틸리티
-- ---------------------------------------------------------------------------

-- | Calculate the height of the tree given total number of leaves.
calcTreeHeight :: Word32 -> Int
calcTreeHeight 1 = 0
calcTreeHeight n = 1 + calcTreeHeight ((n + 1) `div` 2)

-- | Calculate number of nodes at a given height (distance from root).
-- Height 0 is leaves, height 'treeHeight' is root.
-- NOTE: In our traverseTree, height is distance FROM LEAVES.
calcTreeNodes :: Int -> Int -> Word32
calcTreeNodes totalLeaves distFromLeaves =
    fromIntegral $ (totalLeaves + (1 `shiftL` distFromLeaves) - 1) `div` (1 `shiftL` distFromLeaves)
  where
    shiftL :: Int -> Int -> Int
    shiftL v n = v * (2 ^ n)
