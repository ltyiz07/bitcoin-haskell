module Bitcoin.Keys.WIF
    ( Network(..)
    , WIFFormat(..)
    , encodeWIF
    , decodeWIF
    ) where

import qualified Data.ByteString as B
import Data.Word (Word8)

import Utils.Base58
import Utils.Arithmetic
import Utils.Hash
import Bitcoin.Keys.SEC (SECFormat(..))


data Network = Mainnet | Testnet
    deriving (Show, Eq)

data WIFFormat = WIFFormat
    { network    :: Network
    , secFormat  :: SECFormat
    , privateKey :: Integer
    } deriving (Show, Eq)

networkPrefix :: Network -> Word8
networkPrefix Mainnet = 0x80
networkPrefix Testnet = 0xEF

secSuffix :: SECFormat -> B.ByteString
secSuffix Compressed = B.singleton 0x01
secSuffix Uncompressed = B.empty

encodeWIF :: WIFFormat -> B.ByteString
encodeWIF (WIFFormat network secFormat privateKey) = encodeBase58 payloadWithChecksum
  where
    prefix              = networkPrefix network
    keyBytes            = intToBytes32 privateKey
    suffix              = secSuffix secFormat
    payload             = B.cons prefix (keyBytes <> suffix)
    checksum            = B.take 4 (hash256 payload)
    payloadWithChecksum = payload <> checksum

decodeWIF :: B.ByteString -> Either String WIFFormat
decodeWIF wifStr = do
    fullBytes <- decodeBase58 wifStr
    if B.length fullBytes < 37
        then Left "decodeWIF: WIF too short"
        else return ()
    let (payload, checksum) = B.splitAt (B.length fullBytes - 4) fullBytes
    if checksum /= B.take 4 (hash256 payload)
        then Left "decodeWIF: Invalid WIF checksum"
        else return ()
    network <- case B.head payload of
        0x80 -> Right Mainnet
        0xEF -> Right Testnet
        b    -> Left $ "decodeWIF: Unknown WIF prefix: " ++ show b
    let keyPart = B.tail payload
    case B.length keyPart of
        32 -> Right $ WIFFormat network Uncompressed (bytesToInteger keyPart)
        33 -> if B.last keyPart == 0x01
              then Right $ WIFFormat network Compressed (bytesToInteger (B.init keyPart))
              else Left "decodeWIF: Invalid WIF compression flag"
        _  -> Left "decodeWIF: Invalid WIF payload length"
