module Bitcoin.Network.Message.Payload.Block
    ( Block(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import Control.Monad (replicateM, forM_)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putWord64le
    , putByteString
    , getWord32le
    , getWord64le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))
import Bitcoin.Network.Message.Payload.Headers (BlockHeader(..))
import Bitcoin.Network.Message.Payload.Tx (Tx(..))


data Block = Block
    { blockHeader :: BlockHeader
    , blockTxs    :: [Tx]
    } deriving (Show, Eq)

instance Serialize Block where
    put b = do
        put b.blockHeader
        put $ VarInt (fromIntegral $ length b.blockTxs)
        forM_ b.blockTxs put
    get = do
        header <- get
        VarInt txCount <- get
        txs <- replicateM (fromIntegral txCount) get
        return $ Block header txs

instance MessagePayload Block where
    getCommand _ = "block"
