module Bitcoin.Network.Message.Payload.VerAck
    ( VerAck(..)
    ) where

import Data.Serialize (Serialize(..))

import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

data VerAck = VerAck deriving (Show, Eq)

instance Serialize VerAck where
    put _ = return ()
    get = return VerAck

instance MessagePayload VerAck where
    getCommand _ = "verack"
