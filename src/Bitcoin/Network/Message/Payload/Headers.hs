module Bitcoin.Network.Message.Payload.Headers
    ( Headers(..)
    , BlockHeader(..)
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


data BlockHeader = BlockHeader
    { bhVersion    :: Word32         -- ^ 블록 버전
    , bhPrevBlock  :: BS.ByteString  -- ^ 이전 블록 해시 (32바이트)
    , bhMerkleRoot :: BS.ByteString  -- ^ 머클 루트 해시 (32바이트)
    , bhTimestamp  :: Word32         -- ^ 타임스탬프
    , bhBits       :: Word32         -- ^ 난이도 목표 (nBits)
    , bhNonce      :: Word32         -- ^ 논스
    } deriving (Show, Eq)

instance Serialize BlockHeader where
    put bh = do
        putWord32le  bh.bhVersion
        putByteString bh.bhPrevBlock
        putByteString bh.bhMerkleRoot
        putWord32le  bh.bhTimestamp
        putWord32le  bh.bhBits
        putWord32le  bh.bhNonce
        put $ VarInt 0
    get = do
        ver   <- getWord32le
        prev  <- getByteString 32
        root  <- getByteString 32
        time  <- getWord32le
        bits  <- getWord32le
        nonce <- getWord32le
        _ :: VarInt <- get
        return $ BlockHeader ver prev root time bits nonce

newtype Headers = Headers
    { headersList :: [BlockHeader]
    } deriving (Show, Eq)

instance Serialize Headers where
    put hs = do
        put $ VarInt (fromIntegral $ length hs.headersList)
        forM_ hs.headersList put
    get = do
        VarInt count <- get
        hdrs <- replicateM (fromIntegral count) get
        return $ Headers hdrs

instance MessagePayload Headers where
    getCommand _ = "headers"

