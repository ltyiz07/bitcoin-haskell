{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.Message.Header
    ( Message(..)
    , Command(..)
    , MessageHeader(..)
    , Network(..)
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
    )

import Utils.Hash ( hash256 )


data Network = Mainnet | Testnet3 | Regtest | Signet
    deriving (Show, Eq)

magicBytes :: Network -> Word32
magicBytes Mainnet  = 0xf9beb4d9
magicBytes Testnet3 = 0x0b110907
magicBytes Regtest  = 0xfabfb5da
magicBytes Signet   = 0x0a03cf40

magicNetwork :: Word32 -> Either String Network
magicNetwork 0xf9beb4d9 = Right Mainnet
magicNetwork 0x0b110907 = Right Testnet3
magicNetwork 0xfabfb5da = Right Regtest
magicNetwork 0x0a03cf40 = Right Signet
magicNetwork unknown    = Left $ "알 수 없는 매직 바이트: " ++ show unknown

instance Serialize Network where
    put = putWord32be . magicBytes
    get = do
        magic <- getWord32be
        either fail return (magicNetwork magic)

newtype Command = Command { unCommand :: BS.ByteString }
    deriving (Show, Eq)

instance Serialize Command where
    put (Command cmd) = putByteString $ BS.take 12 (cmd <> BS.replicate 12 0)
    get = do
        bs <- getByteString 12
        return $ Command $ BS.takeWhile (/= 0) bs

data MessageHeader = MessageHeader
    { network :: Network
    , command :: Command
    } deriving (Show, Eq)


data Message = Message
    { header :: MessageHeader
    , payload :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize Message where
    put msg = do
        put msg.header.network
        put msg.header.command
        putWord32le $ fromIntegral (BS.length msg.payload)
        let checksum = BS.take 4 (hash256 msg.payload)
        putByteString checksum
        putByteString msg.payload
    get = do
        net <- get
        cmd <- get
        len <- getWord32le
        chk <- getByteString 4
        payload <- getByteString (fromIntegral len)
        let expectedChk = BS.take 4 (hash256 payload)
        unless (chk == expectedChk) $
            fail "Invalid checksum in message header"
        return $ Message (MessageHeader net cmd) payload
