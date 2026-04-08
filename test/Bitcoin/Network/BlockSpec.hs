{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bitcoin.Network.BlockSpec (spec) where

import qualified Test.Hspec as H
import Data.ByteString as BS
import Data.Serialize                  (runGet, get, runPut, put, Serialize)

import Bitcoin.Network.Message
import Utils.Hash (hash256, sha256)
import Utils.Hex (bytesToHex)


build :: Serialize a => a -> BS.ByteString
build a = runPut . put $ a

spec :: H.Spec
spec = do
    H.describe "Test bitcoin blocks" $ do
        H.it "test" $ do
            let fileName = "data/res_header_202604052232.dat"
            content <- BS.readFile fileName 
            let parseResult :: Either String Headers = runGet get content
            case parseResult of
                Left _ -> print "parse failed"
                Right (Headers headers) -> do
                    let (first:first_) = headers
                        (second:second_) = first_
                        (third:third_) = second_
                        headersByte = fmap ((BS.take 80) . build) headers
                    print $ bytesToHex first.bhPrevBlock
                    print $ bytesToHex second.bhPrevBlock
                    print $ bytesToHex third.bhPrevBlock
                    print $ bytesToHex (BS.take 80 (build second))
                    print $ bytesToHex (hash256 (BS.take 80 (build second)))
                    let zeros = BS.replicate 32 0x00
                    print $ bytesToHex zeros
                    print $ bytesToHex (hash256 zeros)
                    print $ bytesToHex (hash256 BS.empty)
