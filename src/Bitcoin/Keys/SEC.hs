module Bitcoin.Keys.SEC
    ( SEC(..)
    , putSEC
    , getSEC
    ) where

import qualified Data.ByteString as BS
import qualified Data.Serialize  as SR


-- | SEC 공개키 구조체
data SEC = Uncompressed Integer Integer  -- ^ X, Y 좌표
         | Compressed Integer Bool       -- ^ X 좌표, Y의 짝수 여부 (True = 짝수, False = 홀수)
         deriving (Show, Eq)

-- | Big-Endian ByteString을 Integer로 변환
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' (\acc b -> (acc * 256) + fromIntegral b) 0

-- | Integer를 정확히 32바이트 길이의 ByteString으로 변환 (앞부분 0 패딩)
integerToBS32 :: Integer -> BS.ByteString
integerToBS32 n =
    let bs = integerToBS n
        len = BS.length bs
    in if len < 32
       then BS.replicate (32 - len) 0 <> bs
       else BS.drop (len - 32) bs

-- | Integer를 Big-Endian ByteString으로 변환
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.empty
integerToBS n = BS.pack $ reverse $ go n
  where
    go 0 = []
    go x = fromIntegral (x `mod` 256) : go (x `div` 256)

instance SR.Serialize SEC where
    put (Uncompressed x y) = do
        SR.putWord8 0x04
        SR.putByteString (integerToBS32 x)
        SR.putByteString (integerToBS32 y)
        
    put (Compressed x isEven) = do
        SR.putWord8 (if isEven then 0x02 else 0x03)
        SR.putByteString (integerToBS32 x)

    get = do
        marker <- SR.getWord8
        case marker of
            0x04 -> do
                xBytes <- SR.getByteString 32
                yBytes <- SR.getByteString 32
                return $ Uncompressed (bsToInteger xBytes) (bsToInteger yBytes)
            0x02 -> do
                xBytes <- SR.getByteString 32
                return $ Compressed (bsToInteger xBytes) True
            0x03 -> do
                xBytes <- SR.getByteString 32
                return $ Compressed (bsToInteger xBytes) False
            _ -> fail $ "Invalid SEC marker: " ++ show marker


putSEC :: SEC -> BS.ByteString
putSEC = SR.runPut . SR.put

getSEC :: BS.ByteString -> Either String SEC
getSEC = SR.runGet SR.get

