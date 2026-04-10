module Bitcoin.Network.Message.Payload.GetData
    ( GetData(..)
    , InvVector(..)
    , InvType(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Control.Monad (replicateM, forM_)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putByteString
    , getWord32le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))
import Bitcoin.Network.Message.Payload.MessagePayload (MessagePayload(..))

-- | 인벤토리 타입 (요청할 데이터의 종류)
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

-- | 특정 데이터의 고유 식별자
data InvVector = InvVector
    { invType :: InvType
    , invHash :: BS.ByteString -- ^ 요청할 데이터의 해시 (32바이트, 리틀 엔디언)
    } deriving (Show, Eq)

instance Serialize InvVector where
    put iv = do
        putWord32le (invTypeToWord32 iv.invType)
        putByteString iv.invHash
    get = do
        t <- getWord32le
        h <- getByteString 32
        return $ InvVector (word32ToInvType t) h

-- | GetData 페이로드
newtype GetData = GetData
    { invVectors :: [InvVector]
    } deriving (Show, Eq)

instance Serialize GetData where
    put gd = do
        put $ VarInt (fromIntegral $ length gd.invVectors)
        forM_ gd.invVectors put
    get = do
        VarInt count <- get
        invs <- replicateM (fromIntegral count) get
        return $ GetData invs

instance MessagePayload GetData where
    getCommand _ = "getdata"
