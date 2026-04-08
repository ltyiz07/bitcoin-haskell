module Bitcoin.Network.Message.Payload.PingPong
    ( Pong(..)
    ) where

import Data.Word (Word64)
import Data.ByteString as BS
import Data.Serialize
    ( Serialize(..)
    , getWord64le
    , putWord64le, putByteString, getByteString
    )

import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))


{-
data Ping = Ping
    { nonce :: Word64
    } deriving (Show, Eq)

instance Serialize Ping where
    put p = putWord64le p.nonce
    get   = Ping <$> getWord64le

instance MessagePayload Ping where
    getCommand _ = "ping"
    -}

data Pong = Pong
    { nonce :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize Pong where
    put p = putByteString p.nonce
    get   = Pong <$> getByteString 8

instance MessagePayload Pong where
    getCommand _ = "pong"
