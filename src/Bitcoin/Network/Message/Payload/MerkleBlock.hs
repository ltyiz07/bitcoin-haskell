module Bitcoin.Network.Message.Payload.MerkleBlock
    ( MerkleBlock(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Control.Monad (replicateM, forM_)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putByteString
    , getWord32le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))
import Bitcoin.Network.Message.Payload.Headers (BlockHeader(..))

-- | BIP 37 'merkleblock' message payload.
data MerkleBlock = MerkleBlock
    { mbHeader   :: BlockHeader
    , mbTotalTxs :: Word32
    , mbHashes   :: [BS.ByteString]
    , mbFlags    :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize MerkleBlock where
    put mb = do
        put mb.mbHeader
        putWord32le mb.mbTotalTxs
        put $ VarInt (fromIntegral $ length mb.mbHashes)
        forM_ mb.mbHashes putByteString
        put $ VarInt (fromIntegral $ BS.length mb.mbFlags)
        putByteString mb.mbFlags
    get = do
        header <- get
        totalTxs <- getWord32le
        VarInt hashCount <- get
        hashes <- replicateM (fromIntegral hashCount) (getByteString 32)
        VarInt flagCount <- get
        flags <- getByteString (fromIntegral flagCount)
        return $ MerkleBlock header totalTxs hashes flags

instance MessagePayload MerkleBlock where
    getCommand _ = "merkleblock"
