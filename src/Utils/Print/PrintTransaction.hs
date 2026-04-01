module Utils.Print.PrintTransaction
    ( printTx
    ) where

import qualified Data.ByteString        as B
import Data.Word (Word32, Word64)

import Utils.Hex
import Bitcoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..), isCoinbase, isSegWit, txId)

-- ============================================================
-- 헬퍼
-- ============================================================

satoshiToBTC :: Word64 -> String
satoshiToBTC sat =
    let (btc, frac) = sat `divMod` 100000000
    in show btc ++ "." ++ pad8 (show frac) ++ " BTC"
  where
    pad8 s = replicate (8 - length s) '0' ++ s

-- ============================================================
-- 출력 함수
-- ============================================================

printTx :: Tx -> IO ()
printTx tx = do
    putStrLn $ "┌─────────────────────────────────────────"
    putStrLn $ "│ TRANSACTION"
    putStrLn $ "├─────────────────────────────────────────"
    putStrLn $ "│ TxID     : " ++ show (txId tx)
    putStrLn $ "│ Version  : " ++ show (version tx)
    putStrLn $ "│ SegWit   : " ++ if isSegWit tx then "Yes" else "No"
    putStrLn $ "│ Coinbase : " ++ if isCoinbase tx then "Yes" else "No"
    putStrLn $ "│ Locktime : " ++ show (locktime tx)
    putStrLn $ "│ Inputs   : " ++ show (length (inputs tx))
    putStrLn $ "│ Outputs  : " ++ show (length (outputs tx))
    putStrLn $ "├─────────────────────────────────────────"

    -- Inputs
    putStrLn $ "│ INPUTS"
    mapM_ (printTxIn tx) (zip [0..] (inputs tx))

    -- Outputs
    putStrLn $ "├─────────────────────────────────────────"
    putStrLn $ "│ OUTPUTS"
    mapM_ printTxOut (zip [0..] (outputs tx))

    -- Witness
    if isSegWit tx
        then do
            putStrLn $ "├─────────────────────────────────────────"
            putStrLn $ "│ WITNESS"
            mapM_ printWitness (zip [0..] (witness tx))
        else return ()

    putStrLn $ "└─────────────────────────────────────────"

printTxIn :: Tx -> (Int, TxIn) -> IO ()
printTxIn tx (i, txin) = do
    putStrLn $ "│  [" ++ show i ++ "]"
    if isCoinbase tx
        then
            putStrLn $ "│    Coinbase Input"
        else do
            putStrLn $ "│    PrevHash : " ++ show (txHash (prevOutput txin))
            putStrLn $ "│    PrevIdx  : " ++ show (index (prevOutput txin))
    putStrLn $ "│    ScriptSig: " ++ formatScript (scriptSig txin)
    putStrLn $ "│    Sequence : 0x" ++ bytesToHex (word32ToBS (Bitcoin.Transaction.sequence txin))

printTxOut :: (Int, TxOut) -> IO ()
printTxOut (i, txout) = do
    putStrLn $ "│  [" ++ show i ++ "]"
    putStrLn $ "│    Value    : " ++ show (value txout)
                                  ++ " sat (" ++ satoshiToBTC (value txout) ++ ")"
    putStrLn $ "│    Script   : " ++ formatScript (scriptPubKey txout)
    putStrLn $ "│    Type     : " ++ detectScriptType (scriptPubKey txout)

printWitness :: (Int, [B.ByteString]) -> IO ()
printWitness (i, items) = do
    putStrLn $ "│  Input [" ++ show i ++ "]"
    mapM_ (\(j, item) ->
        putStrLn $ "│    [" ++ show j ++ "] " ++ bytesToHex item
        ) (zip [0..] items)

-- ============================================================
-- 스크립트 타입 감지
-- ============================================================

detectScriptType :: B.ByteString -> String
detectScriptType bs = case B.unpack bs of
    (0x76:0xa9:0x14:_)           -> "P2PKH"
    (0xa9:0x14:_)                -> "P2SH"
    (0x00:0x14:_)                -> "P2WPKH"
    (0x00:0x20:_)                -> "P2WSH"
    (0x51:0x20:_)                -> "P2TR"
    (0x41:_)                     -> "P2PK (Uncompressed)"
    (0x21:_)                     -> "P2PK (Compressed)"
    []                           -> "Empty"
    _                            -> "Unknown"

-- ============================================================
-- 헬퍼
-- ============================================================

formatScript :: B.ByteString -> String
formatScript bs
    | B.null bs = "(empty)"
    | otherwise = bytesToHex bs

word32ToBS :: Word32 -> B.ByteString
word32ToBS w = B.pack
    [ fromIntegral (w `mod` 256)
    , fromIntegral ((w `div` 256) `mod` 256)
    , fromIntegral ((w `div` 65536) `mod` 256)
    , fromIntegral (w `div` 16777216)
    ]
