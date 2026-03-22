module Utils.Hash
    ( hash256
    , hash160
    , hmac256
    , textToByteString
    , byteStringToHexText
    ) where

import Crypto.Hash (hash, SHA256, RIPEMD160, Digest)
import Crypto.MAC.HMAC (hmac, HMAC)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteArray as BA

-- Hash methods

sha256 :: B.ByteString -> Digest SHA256
sha256 = hash

ripemd160 :: B.ByteString -> Digest RIPEMD160
ripemd160 = hash

hash256 :: B.ByteString -> B.ByteString
hash256 = BA.convert . sha256 . BA.convert . sha256

hash160 :: B.ByteString -> B.ByteString
hash160 = BA.convert . ripemd160 . BA.convert . sha256

hmac256 :: B.ByteString -> B.ByteString -> B.ByteString
hmac256 key msg = BA.convert (hmac key msg :: HMAC SHA256)

-- Converters

textToByteString :: T.Text -> B.ByteString
textToByteString = E.encodeUtf8

byteStringToHexText :: B.ByteString -> T.Text
byteStringToHexText = E.decodeUtf8 . B16.encode

