module Bitcoin.Network.Message.Payload.GetHeaders
    ( GetHeaders(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Control.Monad (replicateM)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putByteString
    , getWord32le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))


data GetHeaders = GetHeaders
    { getHeadersVersion  :: Word32
    , getHeadersLocators :: [BS.ByteString]
    , getHeadersHashStop :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize GetHeaders where
    put gh = do
        putWord32le gh.getHeadersVersion
        put $ VarInt (fromIntegral $ length gh.getHeadersLocators)
        mapM_ putByteString gh.getHeadersLocators
        putByteString gh.getHeadersHashStop
    get = do
        ver <- getWord32le
        VarInt count <- get
        locs <- replicateM (fromIntegral count) (getByteString 32)
        stop <- getByteString 32
        return $ GetHeaders ver locs stop

instance MessagePayload GetHeaders where
    getCommand _ = "getheaders"
