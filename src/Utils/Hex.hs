{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Utils.Hex
    ( hexToBytes
    , bytesToHex
    , bytesToHexReversed
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC


hexToBytes :: String -> B.ByteString
hexToBytes s = case B16.decode (BC.pack s) of
    Left err -> error ("Hex decoding failed: " ++ err)
    Right val -> val

bytesToHex :: B.ByteString -> String
bytesToHex = BC.unpack . B16.encode

bytesToHexReversed :: B.ByteString -> String
bytesToHexReversed = bytesToHex . B.reverse
