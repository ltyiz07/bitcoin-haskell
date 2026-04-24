module Bitcoin.Network.Message.Payload.FilterClear
    ( FilterClear(..)
    ) where

import Data.Serialize (Serialize(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

-- | BIP 37 'filterclear' message payload (empty).
data FilterClear = FilterClear deriving (Show, Eq)

instance Serialize FilterClear where
    put _ = return ()
    get = return FilterClear

instance MessagePayload FilterClear where
    getCommand _ = "filterclear"
