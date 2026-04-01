module Bitcoin.Transaction
    ( OutPoint(..)
    , TxIn(..)
    , TxOut(..)
    , Tx(..)
    , isCoinbase
    , isSegWit
    , txId
    , wtxId
    ) where

import Control.Monad ( unless, replicateM )
import qualified Data.ByteString as B
import Data.Serialize ( Serialize(..), Get, Put )
import Data.Serialize.Put ( putByteString, putWord8, putWord32le, putWord64le, runPut )
import Data.Serialize.Get ( getByteString, getWord8, getWord32le, getWord64le, lookAhead )
import Data.Word ( Word32, Word64 )

import Utils.Hex
import Utils.Hash ( hash256 )
import Bitcoin.VarInt ( VarInt(..) )


newtype Hash32 = Hash32 { unHash32 :: B.ByteString }
    deriving (Eq)

instance Show Hash32 where
    show = bytesToHexReversed . unHash32

instance Serialize Hash32 where
    put (Hash32 h) = putByteString h
    get = do
        bs <- getByteString 32
        return $ Hash32 bs

data OutPoint = OutPoint
    { txHash :: Hash32
    , index  :: Word32
    } deriving (Show, Eq)

instance Serialize OutPoint where
    put (OutPoint h i) = put h >> putWord32le i
    get                = OutPoint <$> get <*> getWord32le

data TxIn = TxIn
    { prevOutput :: OutPoint
    , scriptSig  :: B.ByteString
    , sequence   :: Word32
    } deriving (Show, Eq)

instance Serialize TxIn where
    put (TxIn prev sig seq') = do
        put prev
        put $ VarInt (fromIntegral $ B.length sig)
        putByteString sig
        putWord32le seq'
    get = do
        prev       <- get
        VarInt len <- get
        sig        <- getByteString (fromIntegral len)
        seq'       <- getWord32le
        return $ TxIn prev sig seq'

data TxOut = TxOut
    { value        :: Word64
    , scriptPubKey :: B.ByteString
    } deriving (Show, Eq)

instance Serialize TxOut where
    put (TxOut val spk) = do
        putWord64le val
        put $ VarInt (fromIntegral $ B.length spk)
        putByteString spk
    get = do
        val        <- getWord64le
        VarInt len <- get
        spk        <- getByteString (fromIntegral len)
        return $ TxOut val spk

data Tx = Tx
    { version  :: Word32
    , inputs   :: [TxIn]
    , outputs  :: [TxOut]
    , witness  :: [[B.ByteString]]
    , locktime :: Word32
    } deriving (Show, Eq)

instance Serialize Tx where
    put tx
        | isSegWit tx = putSegWit tx
        | otherwise   = putLegacy tx
    get = do
        ver    <- getWord32le
        marker <- lookAhead getWord8
        if marker == 0x00
            then getSegWit ver
            else getLegacy ver

putLegacy :: Tx -> Put
putLegacy (Tx ver ins outs _ lock) = do
    putWord32le ver
    putVarList ins
    putVarList outs
    putWord32le lock

putSegWit :: Tx -> Put
putSegWit (Tx ver ins outs wit lock) = do
    putWord32le ver
    putWord8 0x00
    putWord8 0x01
    putVarList ins
    putVarList outs
    mapM_ putWitness wit
    putWord32le lock
  where
    putWitness items = do
        put $ VarInt (fromIntegral $ length items)
        mapM_ putWitnessItem items
    putWitnessItem bs = do
        put $ VarInt (fromIntegral $ B.length bs)
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
isSegWit = not . null . witness

isCoinbase :: Tx -> Bool
isCoinbase tx = case inputs tx of
    [TxIn (OutPoint h idx) _ _] ->
        h == Hash32 (B.replicate 32 0x00) && idx == 0xFFFFFFFF
    _ -> False

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
    | isCoinbase tx = Hash32 (B.replicate 32 0x00)
    | otherwise     = Hash32 . hash256 . runPut . put $ tx
