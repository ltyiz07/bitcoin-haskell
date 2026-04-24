module Bitcoin.Network.Message.Payload.FilterLoad
    ( FilterLoad(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Data.Serialize
    ( Serialize(..)
    , putWord8
    , putWord32le
    , putByteString
    , getWord8
    , getWord32le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

-- | BIP 37 'filterload' message payload.
data FilterLoad = FilterLoad
    { flFilter     :: BS.ByteString -- ^ The filter itself
    , flHashFuncs  :: Word32        -- ^ Number of hash functions
    , flTweak      :: Word32        -- ^ Random tweak to randomize the hash
    , flFlags      :: Word8         -- ^ Matching flags
    } deriving (Show, Eq)

instance Serialize FilterLoad where
    put fl = do
        put $ VarInt (fromIntegral $ BS.length fl.flFilter)
        putByteString fl.flFilter
        putWord32le fl.flHashFuncs
        putWord32le fl.flTweak
        putWord8 fl.flFlags
    get = do
        VarInt filterLen <- get
        filterData <- getByteString (fromIntegral filterLen)
        hashFuncs <- getWord32le
        tweak <- getWord32le
        flags <- getWord8
        return $ FilterLoad filterData hashFuncs tweak flags

instance MessagePayload FilterLoad where
    getCommand _ = "filterload"
