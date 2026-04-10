module Bitcoin.Network.Message.Payload.Tx
    ( Tx(..)
    , TxIn(..)
    , TxOut(..)
    , OutPoint(..)
    , Hash32(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word                    (Word32, Word64)
import Control.Monad                (replicateM, unless)
import Data.Serialize
    ( Serialize(..)
    , putWord8
    , getWord8
    , putWord32le
    , getWord32le
    , putWord64le
    , getWord64le
    , putByteString
    , getByteString
    , lookAhead
    , runPut
    , Put
    , Get
    )

import Bitcoin.VarInt               (VarInt(..))
import Utils.Hash                   (hash256)
import Utils.Hex                    (bytesToHexReversed)


newtype Hash32 = Hash32 { unHash32 :: BS.ByteString }
    deriving (Eq)

instance Show Hash32 where
    show = bytesToHexReversed . unHash32

instance Serialize Hash32 where
    put (Hash32 h) = putByteString h
    get = do
        bs <- getByteString 32
        return $ Hash32 bs

data OutPoint = OutPoint
    { hash  :: Hash32    -- ^ 참조하는 이전 트랜잭션 해시 (32바이트)
    , index :: Word32    -- ^ 참조하는 출력(Output)의 인덱스 번호
    } deriving (Show, Eq)

instance Serialize OutPoint where
    put op = put op.hash >> putWord32le op.index
    get    = OutPoint <$> get <*> getWord32le 

data TxIn = TxIn
    { previousOutput  :: OutPoint
    , signatureScript :: BS.ByteString
    , sequence        :: Word32
    } deriving (Show, Eq)

instance Serialize TxIn where
    put ti = do
        put ti.previousOutput
        put $ VarInt (fromIntegral $ BS.length ti.signatureScript)
        putByteString ti.signatureScript
        putWord32le ti.sequence
    get = do
        op <- get
        VarInt scriptLen <- get
        script <- getByteString (fromIntegral scriptLen)
        seqNo <- getWord32le
        return $ TxIn op script seqNo

data TxOut = TxOut
    { value    :: Word64        -- ^ 사토시 단위의 금액
    , pkScript :: BS.ByteString
    } deriving (Show, Eq)

instance Serialize TxOut where
    put to = do
        putWord64le to.value
        put $ VarInt (fromIntegral $ BS.length to.pkScript)
        putByteString to.pkScript
    get = do
        val <- getWord64le
        VarInt scriptLen <- get
        script <- getByteString (fromIntegral scriptLen)
        return $ TxOut val script

data Tx = Tx
    { txVersion  :: Word32
    , txInputs   :: [TxIn]
    , txOutputs  :: [TxOut]
    , txWitness  :: [[BS.ByteString]]
    , txLockTime :: Word32
    } deriving (Show, Eq)

instance Serialize Tx where
    put tx | isSegWit tx = putSegWit tx
           | otherwise   = putLegacy tx

    get = do
        ver    <- getWord32le
        marker <- lookAhead getWord8
        if marker == 0x00
            then getSegWit ver
            else getLegacy ver

putLegacy :: Tx -> Put
putLegacy tx = do
    putWord32le tx.txVersion
    putVarList tx.txInputs
    putVarList tx.txOutputs
    putWord32le tx.txLockTime 

putSegWit :: Tx -> Put
putSegWit tx = do
    putWord32le tx.txVersion 
    putWord8 0x00
    putWord8 0x01
    putVarList tx.txInputs
    putVarList tx.txOutputs
    mapM_ putWitness tx.txWitness
    putWord32le tx.txLockTime
  where
    putWitness items = do
        put $ VarInt (fromIntegral $ length items)
        mapM_ putWitnessItem items
    putWitnessItem bs = do
        put $ VarInt (fromIntegral $ BS.length bs)
        putByteString bs

getLegacy :: Word32 -> Get Tx
getLegacy ver = do
    ins  <- getVarList
    outs <- getVarList
    lock <- getWord32le
    return $ Tx ver ins outs [] lock

getSegWit :: Word32 -> Get Tx
getSegWit ver = do
    marker <- getWord8
    flag   <- getWord8
    unless (marker == 0x00 && flag == 0x01) $
        fail $ "Invalid SegWit marker/flag: expected 00 01, got " ++ show marker ++ " " ++ show flag
    ins     <- getVarList
    outs    <- getVarList
    wit     <- replicateM (length ins) getWitness
    lock    <- getWord32le
    return $ Tx ver ins outs wit lock
  where
    getWitness = do
        VarInt count <- get
        replicateM (fromIntegral count) getWitnessItem
    getWitnessItem = do
        VarInt len <- get
        getByteString (fromIntegral len)

isSegWit :: Tx -> Bool
isSegWit = not . null . txWitness

putVarList :: Serialize a => [a] -> Put
putVarList xs = do
    put $ VarInt (fromIntegral $ length xs)
    mapM_ put xs

getVarList :: Serialize a => Get [a]
getVarList = do
    VarInt count <- get
    if count > 0x10000
        then fail $ "VarList: unreasonably large count: " ++ show count
        else replicateM (fromIntegral count) get


txId :: Tx -> Hash32
txId = Hash32 . hash256 . runPut . putLegacy

wtxId :: Tx -> Hash32
wtxId tx
    | isCoinbase tx = Hash32 (BS.replicate 32 0x00)
    | otherwise     = Hash32 . hash256 . runPut . put $ tx
isCoinbase :: Tx -> Bool
isCoinbase tx = case tx.txInputs of
    [TxIn (OutPoint h idx) _ _] ->
        h == Hash32 (BS.replicate 32 0x00) && idx == 0xFFFFFFFF
    _ -> False

