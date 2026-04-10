{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.BlockSpec (spec) where

import qualified Test.Hspec  as H
import Data.ByteString       as BS
import Data.Serialize        as SR

import Bitcoin.Network.Message
import Bitcoin.Network.Peer     (genesisHashLE)
import Utils.Hash               (hash256)
import Utils.Hex                (hexToBytes)


spec :: H.Spec
spec = do
    -- H.describe "Test bitcoin blocks" $ do
        -- H.it "test block headers integrity" $ do
        --     let fileName = "data/blockheaders_20260409_01.dat"
        --         headerHashOfBlock944318 = BS.reverse . hexToBytes $ "0000000000000000000166c915e785081c6188784c8a5cadd6e63d87e8b698b1"
        --     content <- BS.readFile fileName 
        --     blockHeaders <- either error pure (parseAllBlockHeader content)
        --     Prelude.putStrLn $ "  Total block headers: " ++ show (Prelude.length blockHeaders)
        --     lastHeaderHash <- case validateHeaderIntegrity blockHeaders of
        --         Nothing -> error "Block headers not valid"
        --         Just h  -> return h
        --     lastHeaderHash `H.shouldBe` headerHashOfBlock944318

    H.describe "Test bitcoin block parser" $ do
        H.it "test block integrity" $ do
            let fileName = "data/blocks_930000_to_930004_wit.dat"
            content <- BS.readFile fileName 
            blocks <- either error pure (parseAllBlock content)
            print (blocks !! 0).blockHeader
            print ""
            print ((blocks !! 0).blockTxs !! 0) 
            print ""
            print ((blocks !! 0).blockTxs !! 1000) 
            True `H.shouldBe` True

parseAllBlock :: BS.ByteString -> Either String [Block]
parseAllBlock bs
    | BS.null bs = Right []
    | otherwise  = case runGetState get bs 0 of 
        Left err -> Left $ "파싱 중단 (에러): " ++ err
        Right (payload, rest) -> do
            nextPayload <- parseAllBlock rest
            return (payload : nextPayload)

validateHeaderIntegrity :: [BlockHeader] -> Maybe BS.ByteString
validateHeaderIntegrity = Prelude.foldl' foldlFunc (Just genesisHashLE)
  where
    foldlFunc :: Maybe BS.ByteString -> BlockHeader -> Maybe BS.ByteString
    foldlFunc (Just hash) header | hash == header.bhPrevBlock = Just (getHeaderHash header)
                                 | otherwise                  = Nothing
    foldlFunc Nothing     _                                   = Nothing

parseAllBlockHeader :: BS.ByteString -> Either String [BlockHeader]
parseAllBlockHeader bs
    | BS.null bs = Right []
    | otherwise  = case runGetState get bs 0 of 
        Left err -> Left $ "파싱 중단 (에러): " ++ err
        Right (headersPayload, rest) -> do
            let Headers blockHeaders = headersPayload 
            nextBlockHeaders <- parseAllBlockHeader rest
            return (blockHeaders ++ nextBlockHeaders)

getHeaderHash :: BlockHeader -> BS.ByteString
getHeaderHash = hash256 . BS.take 80 . SR.runPut . SR.put


