module Bitcoin.Keys.WIF
    ( WIF(..)
    , putWIF
    , getWIF
    , toWIFText 
    , fromWIFText
    ) where

import qualified Data.ByteString as BS
import Data.Word                    (Word8)
import qualified Data.Serialize  as SR
import qualified Data.Text       as T
import qualified Data.Text.Encoding as TE

import Utils.Hash   (hash256)
import Utils.Base58 (encodeBase58, decodeBase58)


-- | WIF (Wallet Import Format) 구조체
data WIF = WIF
    { wifPrefix     :: Word8   -- ^ 네트워크 접두사 (Mainnet: 0x80, Testnet: 0xef)
    , wifSecret     :: Integer -- ^ 256비트(32바이트) 개인키
    , wifCompressed :: Bool    -- ^ 압축 공개키 사용 여부 (True일 경우 0x01 접미사 추가)
    } deriving (Show, Eq)

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

instance SR.Serialize WIF where
    put (WIF prefix secret compressed) = do
        SR.putWord8 prefix
        SR.putByteString (integerToBS32 secret)
        if compressed
            then SR.putWord8 0x01
            else return ()

    get = do
        prefix <- SR.getWord8
        secretBytes <- SR.getByteString 32
        -- 남은 바이트가 있는지 확인하여 압축 플래그 존재 여부 파악
        empty <- SR.isEmpty
        compressed <- if empty
                      then return False
                      else do
                          flag <- SR.getWord8
                          if flag == 0x01
                              then return True
                              else fail $ "Invalid WIF compression flag: " ++ show flag
        return $ WIF prefix (bsToInteger secretBytes) compressed


putWIF :: WIF -> BS.ByteString
putWIF = SR.runPut . SR.put

getWIF :: BS.ByteString -> Either String WIF
getWIF = SR.runGet SR.get

-- | WIF 구조체를 Base58Check 인코딩된 문자열로 변환
toWIFText :: WIF -> T.Text
toWIFText wif =
    let payload = SR.encode wif
        -- 페이로드의 이중 SHA-256 해시값 중 첫 4바이트를 체크섬으로 사용
        checksum = BS.take 4 (hash256 payload)
        -- 페이로드와 체크섬을 합쳐 Base58 인코딩 후 Text로 디코딩
    in TE.decodeUtf8 $ encodeBase58 (payload <> checksum)

-- | Base58Check 문자열을 파싱하여 WIF 구조체로 복원
fromWIFText:: T.Text -> Either String WIF
fromWIFText txt = do
    -- 1. Text를 ByteString으로 변환 후 Base58 디코딩
    -- (decodeBase58이 이미 Either String을 반환하므로 case 패턴 매칭 생략)
    bytes <- decodeBase58 (TE.encodeUtf8 txt)
    
    let len = BS.length bytes
    if len < 4
        then Left "Data too short for Base58Check"
        else do
            -- 2. 페이로드와 체크섬 분리
            let (payload, checksum) = BS.splitAt (len - 4) bytes
                expectedChecksum = BS.take 4 (hash256 payload)
            
            -- 3. 체크섬 검증
            if checksum /= expectedChecksum
                then Left "WIF checksum mismatch"
                else 
                    -- 4. Serialize 인스턴스를 통해 페이로드 역직렬화
                    SR.decode payload
