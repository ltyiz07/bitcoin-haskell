module Bitcoin.Network.BlockSpec (spec) where

import qualified Test.Hspec as H
import Data.ByteString as BS

import Bitcoin.Network.Message


spec :: H.Spec
spec = do
    H.describe "Test bitcoin blocks" $ do
        H.it "test" $ do
            let fileName = "data/res_header_202604052232.dat"
            -- content <- BS.readFile fileName 
            -- print content
            True `H.shouldBe` True
