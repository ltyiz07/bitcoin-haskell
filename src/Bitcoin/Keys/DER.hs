module Bitcoin.Keys.DER
    ( DER(..)
    , putDER
    , getDER
    ) where

import Control.Monad                (when)
import qualified Data.ByteString as BS
import qualified Data.Serialize  as SR

data DER = DER
    { derR :: Integer
    , derS :: Integer
    } deriving (Show, Eq)

-- | Big-Endian ByteString을 Integer로 변환
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' (\acc b -> (acc * 256) + fromIntegral b) 0

-- | Integer를 Big-Endian ByteString으로 변환 (불필요한 앞의 0 제거)
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.singleton 0x00
integerToBS n = BS.pack $ reverse $ go n
  where
    go 0 = []
    go x = fromIntegral (x `mod` 256) : go (x `div` 256)

-- | BIP 66 규칙에 따른 DER Integer 인코딩
-- 양수임을 보장하기 위해 첫 바이트가 0x80 이상일 경우 0x00을 앞에 추가합니다.
encodeDerInt :: Integer -> BS.ByteString
encodeDerInt n =
    let bs = integerToBS n
    in if BS.length bs > 0 && BS.head bs >= 0x80
       then BS.cons 0x00 bs
       else bs

instance SR.Serialize DER where
    put (DER r s) = do
        let rBytes = encodeDerInt r
            sBytes = encodeDerInt s
            -- 0x02 (Integer Tag) + Length + Payload
            rPayload = BS.cons 0x02 $ BS.cons (fromIntegral $ BS.length rBytes) rBytes
            sPayload = BS.cons 0x02 $ BS.cons (fromIntegral $ BS.length sBytes) sBytes
            totalPayload = rPayload <> sPayload
        -- 0x30 (Sequence Tag) + Total Length + Payloads
        SR.putWord8 0x30
        SR.putWord8 (fromIntegral $ BS.length totalPayload)
        SR.putByteString totalPayload

    get = do
        seqTag <- SR.getWord8
        when (seqTag /= 0x30) $ fail "Invalid DER sequence tag (expected 0x30)"
        _totalLen <- SR.getWord8 -- 전체 길이는 검증에서 제외하거나 활용 가능
        -- Parse r
        rTag <- SR.getWord8
        when (rTag /= 0x02) $ fail "Invalid DER integer tag for r (expected 0x02)"
        rLen <- SR.getWord8
        rBytes <- SR.getByteString (fromIntegral rLen)
        -- Parse s
        sTag <- SR.getWord8
        when (sTag /= 0x02) $ fail "Invalid DER integer tag for s (expected 0x02)"
        sLen <- SR.getWord8
        sBytes <- SR.getByteString (fromIntegral sLen)
        return $ DER (bsToInteger rBytes) (bsToInteger sBytes)

putDER :: DER -> BS.ByteString
putDER = SR.runPut . SR.put

getDER :: BS.ByteString -> Either String DER
getDER = SR.runGet SR.get

