{-# LANGUAGE OverloadedStrings #-}
module Bitcoin.MerkleTreeSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import Bitcoin.MerkleTree
import Utils.Hash (hash256)

spec :: Spec
spec = do
    describe "MerkleTree" $ do
        it "extractMatches from a simple 1-tx tree" $ do
            let tx1 = hash256 "tx1"
                -- Root is Tx1. Flag 1 (matched)
                pmt = PartialMerkleTree 1 [tx1] (BS.singleton 0x01)
            extractMatches pmt `shouldBe` Right (tx1, [tx1])

        it "extractMatches from a 2-tx tree where 2nd is matched" $ do
            let tx1 = hash256 "tx1"
                tx2 = hash256 "tx2"
                root = hash256 (tx1 <> tx2)
                -- Traverse: Root(1), Tx1(0), Tx2(1)
                -- Bits: [1, 0, 1] -> 0x05
                -- Hashes: [Tx1, Tx2]
                pmt = PartialMerkleTree 2 [tx1, tx2] (BS.singleton 0x05)
            extractMatches pmt `shouldBe` Right (root, [tx2])

        it "extractMatches from a 4-tx tree where 2nd is matched" $ do
            let tx1 = hash256 "tx1"
                tx2 = hash256 "tx2"
                tx3 = hash256 "tx3"
                tx4 = hash256 "tx4"
                h12 = hash256 (tx1 <> tx2)
                h34 = hash256 (tx3 <> tx4)
                root = hash256 (h12 <> h34)
                -- Traverse:
                -- Root: 1 (descend)
                --   L: 1 (descend)
                --     LL(Tx1): 0 (not matched) -> Hash: Tx1
                --     LR(Tx2): 1 (matched) -> Hash: Tx2
                --   R: 0 (no descend) -> Hash: H34
                -- Bits: [1, 1, 0, 1, 0] -> 0x0B (00001011)
                -- Hashes: [Tx1, Tx2, h34]
                pmt = PartialMerkleTree 4 [tx1, tx2, h34] (BS.singleton 0x0B)
            extractMatches pmt `shouldBe` Right (root, [tx2])

        it "should fail if not all hashes are consumed" $ do
            let tx1 = hash256 "tx1"
                pmt = PartialMerkleTree 1 [tx1, hash256 "extra"] (BS.singleton 0x01)
            case extractMatches pmt of
                Left err -> err `shouldContain` "남은 해시"
                Right _ -> expectationFailure "Should have failed"
