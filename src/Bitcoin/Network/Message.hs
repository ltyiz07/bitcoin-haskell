module Bitcoin.Network.Message
    ( Message(..)
    , buildMainnetMessage
    ) where

import Control.Monad (unless)
import Data.ByteString as BS
import Data.Word (Word64)
import Data.Serialize
    ( Serialize(..)
    , putWord32be
    , getWord32be
    , putWord32le
    , getWord32le
    , putByteString
    , getByteString 
    , runPut
    )

import Utils.Hash ( hash256 )
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))
import Bitcoin.Network.Message.Header (MessageHeader(..))
import Bitcoin.Network.Message.Payload.Version (Version(..))
import Bitcoin.Network.Message.NetworkMagic (Network(..))
import Bitcoin.Network.Message.Payload.GetHeaders (GetHeaders (..))


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

-- buildMainnetMessage :: Serialize a => BS.ByteString -> a -> Message
-- buildMainnetMessage cmd msg = Message (MessageHeader Mainnet cmd) (runPut . put $ msg)
