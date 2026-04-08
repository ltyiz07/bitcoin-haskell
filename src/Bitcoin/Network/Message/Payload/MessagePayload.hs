module Bitcoin.Network.Message.Payload.MessagePayload
    ( MessagePayload(..)
    ) where

import Data.Serialize (Serialize)
import Data.ByteString as BS

class Serialize a => MessagePayload a where
    getCommand :: a -> BS.ByteString
