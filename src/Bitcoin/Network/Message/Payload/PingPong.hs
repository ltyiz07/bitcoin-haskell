module Bitcoin.Network.Message.Payload.PingPong
    ( Ping(..)
    , Pong(..)
    ) where

import Data.ByteString as BS
import Data.Serialize
    ( Serialize(..)
    , putByteString
    , getByteString
    )

import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))


newtype Ping = Ping
    { pingNonce :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize Ping where
    put p = putByteString p.pingNonce
    get   = Ping <$> getByteString 8

instance MessagePayload Ping where
    getCommand _ = "ping"

newtype Pong = Pong
    { pongNonce :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize Pong where
    put p = putByteString p.pongNonce
    get   = Pong <$> getByteString 8

instance MessagePayload Pong where
    getCommand _ = "pong"
