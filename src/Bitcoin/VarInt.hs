module Bitcoin.VarInt
    ( VarInt(..)
    , encodeVarInt
    , decodeVarInt
    ) where

import qualified Data.ByteString as B
import Data.Serialize
import Data.Word


newtype VarInt = VarInt { unVarInt :: Word64 }
    deriving (Show, Eq)

instance Serialize VarInt where
    put (VarInt n)
        | n < 0xfd        = putWord8 (fromIntegral n)
        | n <= 0xffff     = putWord8 0xfd >> putWord16le (fromIntegral n)
        | n <= 0xffffffff = putWord8 0xfe >> putWord32le (fromIntegral n)
        | otherwise       = putWord8 0xff >> putWord64le n

    get = do
        tag <- getWord8
        case tag of
            0xfd -> VarInt . fromIntegral <$> getWord16le
            0xfe -> VarInt . fromIntegral <$> getWord32le
            0xff -> VarInt                <$> getWord64le
            _    -> return $ VarInt (fromIntegral tag)

encodeVarInt :: Word64 -> B.ByteString
encodeVarInt = runPut . put . VarInt

decodeVarInt :: B.ByteString -> Either String Word64
decodeVarInt bs = unVarInt <$> runGet get bs
