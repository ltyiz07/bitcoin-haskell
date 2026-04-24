{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.BlockSpec (spec) where

import qualified Data.List            as L
import qualified Test.Hspec           as H
import qualified Data.ByteString      as BS
import qualified Data.Serialize       as SR

import Bitcoin.Network.Message
import Bitcoin.Network.Verifier 
import Bitcoin.Network.Peer     (genesisHashLE)
import Utils.Hash               (hash256)
import Utils.Hex                (hexToBytes, bytesToHex)
import Utils.Arithmetic         (bytesToInteger)


spec :: H.Spec
spec = do
    let headersFilename = "data/blockheaders_20260409_01.dat"
        blocksFilename = "data/blocks_930000_to_930004_wit.dat"

    H.describe "Verify bitcoin block headers" $ do
        H.it "test block headers integrity" $ do
            let headerHashOfBlock944318 = BS.reverse . hexToBytes $ "0000000000000000000166c915e785081c6188784c8a5cadd6e63d87e8b698b1"
            content <- BS.readFile headersFilename 
            blockHeaders <- either error pure (parseAllBlockHeader content)
            Prelude.putStrLn $ "  Total block headers: " ++ show (Prelude.length blockHeaders)
            lastHeaderHash <- case verifyHeadersIntegrity genesisHashLE blockHeaders of
                Nothing -> error "Block headers not valid"
                Just h  -> return h
            length blockHeaders `H.shouldBe` 944318
            lastHeaderHash `H.shouldBe` headerHashOfBlock944318

    H.describe "Verify target difficulty" $ do
        H.it "convert between bits and target difficulty" $ do
            content <- BS.readFile headersFilename 
            blockHeaders <- either error pure (parseAllBlockHeader content)
            let sampleBits = (last blockHeaders).bhBits
                target     = bitsToTarget sampleBits
                targetBits = targetToBits target
            targetBits `H.shouldBe` sampleBits 
            let blockHeight = Prelude.length blockHeaders + 1
                (firstHeight, lastHeight) = epochRangeFromHeight blockHeight
                (firstIndex, lastIndex) = (firstHeight - 1, lastHeight - 1)
                oldBits     = (blockHeaders !! lastIndex ).bhBits
                calcedTarget =  calculateNewTarget
                                    (bitsToTarget oldBits)
                                    (blockHeaders !! firstIndex).bhTimestamp
                                    (blockHeaders !! lastIndex).bhTimestamp
                calcedBits  = targetToBits calcedTarget
            calcedBits `H.shouldBe` sampleBits
            let blockHeader = last blockHeaders
                pow = bytesToInteger . BS.reverse . hash256 . SR.runPut . SR.put $ blockHeader
            pow `H.shouldSatisfy` (target >=)

    H.describe "Verify merkle root" $ do
        H.it "generate merkle root" $ do
            content <- BS.readFile blocksFilename 
            blocks <- either error pure (parseAllBlock content)
            let block = last blocks
                header = block.blockHeader
                txs = block.blockTxs
            Hash32 header.bhMerkleRoot `H.shouldBe` getMerkleRoot txs

    H.describe "Verify witness merkle root" $ do
        H.it "generate witness merkle root" $ do
            content <- BS.readFile blocksFilename 
            blocks <- either error pure (parseAllBlock content)
            let block = last blocks
                txs = block.blockTxs
                coinbaseTx = txs !! 0
            isSegWit coinbaseTx `H.shouldBe` True
            isCoinbase coinbaseTx `H.shouldBe` True
            let preBytes = hexToBytes "6a24aa21a9ed"
                commitment = case L.find (\x -> BS.isPrefixOf preBytes x.pkScript) coinbaseTx.txOutputs of
                    Just output -> output.pkScript
                    Nothing -> error "Commitment hash not found"
                sample = preBytes <> hash256 (unHash32 (getWitnessmerkleRoot txs) <> (hexToBytes "0000000000000000000000000000000000000000000000000000000000000000"))
            sample `H.shouldBe` commitment

    H.describe "test transaction hash" $ do
        H.it "get transactino hash" $ do
            let tx1Raw          = hexToBytes "0200000000010292f1ff48ccd7f834ce68c79aff09453a62dc8a96200229be2134f03f62a371d80100000000fdffffff07267cf7be588dd112ce65df52769e95bd27b0f26e3b7904ef6089539798fff41600000000fdffffff0420a1070000000000160014a5760be838efd97c65017977e6c48ac8b43389dd20a1070000000000160014f11789b05aaee3bbf124c493ca456ae3ff7dfe18acfdeb00000000001600143d433bf051470ae73abeaea5a1cdf61fdb9b324220a1070000000000160014bc4f6b38ec77af76e2080b0e7d0733b2bba27aff0247304402206aacf8e1cf33fada414f642c4edf909f29c6eac71cd9d1b3f05a7c2afe75839402201275465d1b47d23e0a7690f8d3c2e39bb845f10f0149fe5e64ccadc465e68878012102a95eec718942d730689fc78301d356f9f96db7e87514e20bebc80e4d2e64f876024730440220019937ee0e9215696b9c224acbe0d01b2ef12d6a5656460a34f41d71ac14d33d022076c338900c7a1a81e103ebbbbbbf236a369b422dabb1edfae383026d8d1d71a80121031856628a41cb111b05efb23ef8e5026536275846acfcef8fe463d3e313e3fc1900000000"
                tx2Raw          = hexToBytes "020000000001018a7f4e7578cfbac69328abf34b882b4b6d5de538f30fbabf164c2377d1393d722600000000fdffffff02df83010000000000160014368f1afd189a5488eb5cf94c8f13ee29a3a0d9b9400d030000000000160014ffe32cdc0c281cb539639eec62977c1aa8b43bf602473044022017722420e7b7611af8da5844664f7b1a5251b09ee885290a9754bbba371a9e8302200d72b863729d43f9cab843ae37dc9efbdb1f72665e82117a189bcccb6f42a4e101210301f457faf6aef1b4904e515ef5c71be61843ae2fc8f834afb761625f239e575d00000000"
                tx1 :: Tx       = case SR.runGet SR.get tx1Raw of
                    Left _ -> error "Parse transaction failed"
                    Right res -> res
                tx2 :: Tx       = case SR.runGet SR.get tx2Raw of 
                    Left _ -> error "Parse transaction failed"
                    Right res -> res
                tx2Hash         = txId tx2
                tx1PrevHash     = (tx1.txInputs !! 0).previousOutput.hash
                tx1PrevIndex    = fromIntegral (tx1.txInputs !! 0).previousOutput.index
            tx2Hash `H.shouldBe` tx1PrevHash 

parseAllBlock :: BS.ByteString -> Either String [Block]
parseAllBlock bs
    | BS.null bs = Right []
    | otherwise  = case SR.runGetState SR.get bs 0 of 
        Left err -> Left $ "파싱 중단 (에러): " ++ err
        Right (payload, rest) -> do
            nextPayload <- parseAllBlock rest
            return (payload : nextPayload)

parseAllBlockHeader :: BS.ByteString -> Either String [BlockHeader]
parseAllBlockHeader bs
    | BS.null bs = Right []
    | otherwise  = case SR.runGetState SR.get bs 0 of 
        Left err -> Left $ "파싱 중단 (에러): " ++ err
        Right (headersPayload, rest) -> do
            let Headers blockHeaders = headersPayload 
            nextBlockHeaders <- parseAllBlockHeader rest
            return (blockHeaders ++ nextBlockHeaders)

