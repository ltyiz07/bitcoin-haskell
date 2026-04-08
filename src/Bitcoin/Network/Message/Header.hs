module Bitcoin.Network.Message.Header
    ( Message(..)
    , MessageHeader(..)
    , Network(..)
    , buildMainnetMessage 
    ) where

import Control.Monad (unless)
import Data.ByteString as BS
import Data.Word
    ( Word32
    )
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


data Network = Mainnet | Testnet3 | Regtest | Signet
    deriving (Show, Eq)

magicBytes :: Network -> Word32
magicBytes Mainnet  = 0xf9beb4d9
magicBytes Testnet3 = 0x0b110907
magicBytes Regtest  = 0xfabfb5da
magicBytes Signet   = 0x0a03cf40

instance Serialize Network where
    put = putWord32be . magicBytes
    get = do
        magic <- getWord32be
        case magic of
            0xf9beb4d9 -> return Mainnet
            0x0b110907 -> return Testnet3
            0xfabfb5da -> return Regtest
            0x0a03cf40 -> return Signet
            unknown    -> fail $ "알 수 없는 매직 바이트: " ++ show unknown

data MessageHeader = MessageHeader
    { network :: Network
    , command :: BS.ByteString
    } deriving (Show, Eq)

data Message = Message
    { header :: MessageHeader
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

buildMainnetMessage :: Serialize a => BS.ByteString -> a -> Message
buildMainnetMessage cmd msg = Message (MessageHeader Mainnet cmd) (runPut . put $ msg)
