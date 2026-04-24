module Bitcoin.Network.Message
    ( Message(..)
    , buildMainnetMessage
    , parseHeader
    , parsePayload 
    , module Bitcoin.Network.Message.NetworkMagic
    , module Bitcoin.Network.Message.Header
    , module Bitcoin.Network.Message.Payload.GetHeaders
    , module Bitcoin.Network.Message.Payload.Version
    , module Bitcoin.Network.Message.Payload.VerAck
    , module Bitcoin.Network.Message.Payload.PingPong
    , module Bitcoin.Network.Message.Payload.Headers
    , module Bitcoin.Network.Message.Payload.GetData
    , module Bitcoin.Network.Message.Payload.InvType
    , module Bitcoin.Network.Message.Payload.Tx
    , module Bitcoin.Network.Message.Payload.Inv
    , module Bitcoin.Network.Message.Payload.Block
    , module Bitcoin.Network.Message.Payload.FilterLoad
    , module Bitcoin.Network.Message.Payload.FilterAdd
    , module Bitcoin.Network.Message.Payload.FilterClear
    , module Bitcoin.Network.Message.Payload.MerkleBlock
    ) where

import Control.Monad (unless)
import Data.ByteString as BS
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , getWord32le
    , putByteString
    , getByteString 
    , runPut
    )

import Utils.Hash ( hash256 )
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

import Bitcoin.Network.Message.NetworkMagic
import Bitcoin.Network.Message.Header
import Bitcoin.Network.Message.Payload.Version
import Bitcoin.Network.Message.Payload.VerAck
import Bitcoin.Network.Message.Payload.PingPong
import Bitcoin.Network.Message.Payload.GetHeaders
import Bitcoin.Network.Message.Payload.Headers
import Bitcoin.Network.Message.Payload.GetData
import Bitcoin.Network.Message.Payload.InvType
import Bitcoin.Network.Message.Payload.Tx
import Bitcoin.Network.Message.Payload.Inv
import Bitcoin.Network.Message.Payload.Block
import Bitcoin.Network.Message.Payload.FilterLoad
import Bitcoin.Network.Message.Payload.FilterAdd
import Bitcoin.Network.Message.Payload.FilterClear
import Bitcoin.Network.Message.Payload.MerkleBlock


data Message = Message 
    { header  :: MessageHeader
    , payload :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize Message where
    put msg = do
        put msg.header.network
        putByteString $ BS.take 12 (msg.header.command <> BS.replicate 12 0)
        putWord32le $ fromIntegral (BS.length msg.payload)
        let checksum = BS.take 4 (hash256 msg.payload)
        putByteString checksum
        putByteString msg.payload
    get = do
        net <- get
        rawCmd <- getByteString 12
        let cmd = BS.takeWhile (/= 0) rawCmd
        len <- getWord32le
        chk <- getByteString 4
        payload <- getByteString (fromIntegral len)
        let expectedChk = BS.take 4 (hash256 payload)
        unless (chk == expectedChk) $
            fail "Invalid checksum in message header"
        return $ Message (MessageHeader net cmd) payload

buildMainnetMessage :: MessagePayload a => a -> Message
buildMainnetMessage payload = Message (MessageHeader Mainnet (getCommand payload)) (runPut . put $ payload)

parseHeader :: BS.ByteString -> Message
parseHeader = undefined 
parsePayload :: MessagePayload a => Message -> a
parsePayload = undefined 

