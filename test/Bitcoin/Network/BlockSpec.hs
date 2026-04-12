{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.BlockSpec (spec) where

import qualified Test.Hspec  as H
import qualified Data.ByteString       as BS
import qualified Data.Serialize        as SR

import Bitcoin.Network.Message
import Bitcoin.Network.Verifier 
import Bitcoin.Network.Peer     (genesisHashLE)
import Utils.Hash               (hash256)
import Utils.Hex                (hexToBytes)


spec :: H.Spec
spec = do
    let headersFilename = "data/blockheaders_20260409_01.dat"
        blocksFilename = "data/blocks_930000_to_930004_wit.dat"

    H.describe "Verify bitcoin block headers" $ do
        H.it "test block headers integrity" $ do
            let headerHashOfBlock944318 = BS.reverse . hexToBytes $ "0000000000000000000166c915e785081c6188784c8a5cadd6e63d87e8b698b1"
                validateHeaderIntegrity :: [BlockHeader] -> Maybe BS.ByteString
                validateHeaderIntegrity = Prelude.foldl' foldlFunc (Just genesisHashLE)
                  where
                    foldlFunc :: Maybe BS.ByteString -> BlockHeader -> Maybe BS.ByteString
                    foldlFunc (Just hash) header | hash == header.bhPrevBlock = Just (getHeaderHash header)
                                                 | otherwise                  = Nothing
                    foldlFunc Nothing     _                                   = Nothing

                    getHeaderHash :: BlockHeader -> BS.ByteString
                    getHeaderHash = hash256 . BS.take 80 . SR.runPut . SR.put

            content <- BS.readFile headersFilename 
            blockHeaders <- either error pure (parseAllBlockHeader content)
            Prelude.putStrLn $ "  Total block headers: " ++ show (Prelude.length blockHeaders)
            lastHeaderHash <- case validateHeaderIntegrity blockHeaders of
                Nothing -> error "Block headers not valid"
                Just h  -> return h
            lastHeaderHash `H.shouldBe` headerHashOfBlock944318

    H.describe "Test bitcoin block parser" $ do
        H.it "test block integrity" $ do
            content <- BS.readFile blocksFilename 
            blocks <- either error pure (parseAllBlock content)
            print (blocks !! 0).blockHeader
            True `H.shouldBe` True

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

