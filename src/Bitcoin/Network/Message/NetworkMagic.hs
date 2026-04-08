module Bitcoin.Network.Message.NetworkMagic
    ( Network(..)
    ) where

import Data.Word ( Word32 )
import Data.Serialize (Serialize(..), getWord32be, putWord32be)


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



