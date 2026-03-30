module Utils.Hash
    ( sha256
    , ripemd160
    , hash256
    , hash160
    , hmac256
    , textToByteString
    , byteStringToHexText
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteArray as BA
import Crypto.Hash (hash, SHA256, RIPEMD160, Digest)
import Crypto.MAC.HMAC (hmac, HMAC)


-- Hash methods
_sha256 :: B.ByteString -> Digest SHA256
_sha256 = hash

_ripemd160 :: B.ByteString -> Digest RIPEMD160
_ripemd160 = hash

sha256 :: B.ByteString -> B.ByteString
sha256 = BA.convert . _sha256

ripemd160 :: B.ByteString -> B.ByteString
ripemd160 = BA.convert . _ripemd160 

hash256 :: B.ByteString -> B.ByteString
hash256 = sha256 . sha256

hash160 :: B.ByteString -> B.ByteString
hash160 = ripemd160 . sha256

hmac256 :: B.ByteString -> B.ByteString -> B.ByteString
hmac256 key msg = BA.convert (hmac key msg :: HMAC SHA256)

-- Converters

textToByteString :: T.Text -> B.ByteString
textToByteString = E.encodeUtf8

byteStringToHexText :: B.ByteString -> T.Text
byteStringToHexText = E.decodeUtf8 . B16.encode

