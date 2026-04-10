module Bitcoin.Network.Message.Payload.Inv
    ( Inv(..)
    ) where

import Control.Monad    (replicateM, forM_)
import Data.Serialize   ( Serialize(..))

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload   (MessagePayload(..))
import Bitcoin.Network.Message.Payload.InvType          (InvVector(..))


newtype Inv = Inv
    { inventory :: [InvVector]
    } deriving (Show, Eq)

instance Serialize Inv where
    put invMsg = do
        put $ VarInt (fromIntegral $ length invMsg.inventory)
        forM_ invMsg.inventory put
    get = do
        VarInt count <- get
        if count > 50000
            then fail $ "Inv 메시지 한도 초과 (최대 50,000개): " ++ show count
            else do
                invs <- replicateM (fromIntegral count) get
                return $ Inv invs

instance MessagePayload Inv where
    getCommand _ = "inv"
