module Bitcoin.Network.Message.Payload.InvType
    ( InvType(..)
    , InvVector(..)
    , invTypeToWord32 
    , word32ToInvType 
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putByteString
    , getWord32le
    , getByteString
    )


data InvType
    = MsgTx             -- ^ 1: 일반 트랜잭션
    | MsgBlock          -- ^ 2: 전체 블록
    | MsgFilteredBlock  -- ^ 3: SPV용 필터링된 블록 (BIP37)
    | MsgCmpctBlock     -- ^ 4: 압축 블록 (BIP152)
    | MsgWitnessTx      -- ^ 0x40000001 (SegWit 트랜잭션)
    | MsgWitnessBlock   -- ^ 0x40000002 (SegWit 블록)
    | UnknownInvType Word32
    deriving (Show, Eq)

invTypeToWord32 :: InvType -> Word32
invTypeToWord32 MsgTx            = 1
invTypeToWord32 MsgBlock         = 2
invTypeToWord32 MsgFilteredBlock = 3
invTypeToWord32 MsgCmpctBlock    = 4
invTypeToWord32 MsgWitnessTx     = 0x40000001
invTypeToWord32 MsgWitnessBlock  = 0x40000002
invTypeToWord32 (UnknownInvType n) = n

word32ToInvType :: Word32 -> InvType
word32ToInvType 1 = MsgTx
word32ToInvType 2 = MsgBlock
word32ToInvType 3 = MsgFilteredBlock
word32ToInvType 4 = MsgCmpctBlock
word32ToInvType 0x40000001 = MsgWitnessTx
word32ToInvType 0x40000002 = MsgWitnessBlock
word32ToInvType n = UnknownInvType n

data InvVector = InvVector
    { invType :: InvType
    , invHash :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize InvVector where
    put iv = do
        putWord32le (invTypeToWord32 iv.invType)
        putByteString iv.invHash
    get = do
        t <- getWord32le
        h <- getByteString 32
        return $ InvVector (word32ToInvType t) h
