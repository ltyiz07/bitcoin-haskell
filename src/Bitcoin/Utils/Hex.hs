module Bitcoin.Utils.Hex
    ( hexToBytes
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16


hexToBytes :: B.ByteString -> B.ByteString
hexToBytes bs = case B16.decode bs of
    Left err -> error ("Hex decoding failed: " ++ err)
    Right val -> val
