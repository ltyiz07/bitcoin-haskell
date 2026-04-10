module Bitcoin.Network.Message.Payload.GetData
    ( GetData(..)
    ) where

import Control.Monad (replicateM, forM_)
import Data.Serialize
    ( Serialize(..)
    )

import Bitcoin.VarInt                                   (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload   (MessagePayload(..))
import Bitcoin.Network.Message.Payload.InvType          (InvVector(..))


newtype GetData = GetData
    { invVectors :: [InvVector]
    } deriving (Show, Eq)

instance Serialize GetData where
    put gd = do
        put $ VarInt (fromIntegral $ length gd.invVectors)
        forM_ gd.invVectors put
    get = do
        VarInt count <- get
        if count > 50000
            then fail $ "GetData 메시지 한도 초과 (최대 50,000개): " ++ show count
            else do
                invs <- replicateM (fromIntegral count) get
                return $ GetData invs

instance MessagePayload GetData where
    getCommand _ = "getdata"
