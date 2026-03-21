module Utils.Hash
    ( hash256
    , hash160
    ) where

import Crypto.Hash (hash, SHA256, RIPEMD160, Digest)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteArray as BA

toByteString :: String -> B.ByteString
toByteString s = E.encodeUtf8 (T.pack s)

toString :: B.ByteString -> String
toString bs = T.unpack (E.decodeUtf8 bs)

toHexString :: B.ByteString -> String
toHexString bs = T.unpack (E.decodeUtf8 (B16.encode bs))

hash256 :: String -> String
hash256 bs =
    let first = hash (toByteString bs) :: Digest SHA256
        second = hash (BA.convert first :: B.ByteString) :: Digest SHA256
    in toHexString (BA.convert second)

hash160 :: Integer -> Integer
hash160 = undefined
