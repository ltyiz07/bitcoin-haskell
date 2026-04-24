module Bitcoin.Network.Message.Payload.FilterAdd
    ( FilterAdd(..)
    ) where

import qualified Data.ByteString as BS
import Data.Serialize
    ( Serialize(..)
    , putByteString
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

-- | BIP 37 'filteradd' message payload.
data FilterAdd = FilterAdd
    { faData :: BS.ByteString -- ^ The data to add to the filter
    } deriving (Show, Eq)

instance Serialize FilterAdd where
    put fa = do
        put $ VarInt (fromIntegral $ BS.length fa.faData)
        putByteString fa.faData
    get = do
        VarInt dataLen <- get
        d <- getByteString (fromIntegral dataLen)
        return $ FilterAdd d

instance MessagePayload FilterAdd where
    getCommand _ = "filteradd"
