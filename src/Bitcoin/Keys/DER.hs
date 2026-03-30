module Bitcoin.Keys.DER
    ( encodeDER
    , decodeDER
    ) where

import qualified Data.ByteString as B
import qualified Data.Serialize  as C

import Utils.Arithmetic (bytesToInteger)


{-
 - (x `quot` y)*y + (x `rem` y) == x  
 - (x `div`  y)*y + (x `mod` y) == x
 -}
integerToDER :: Integer -> B.ByteString
integerToDER 0 = B.singleton 0x00
integerToDER n = 
    let bs = integerToBytesBE n
    in if B.head bs >= 0x80
        then B.cons 0x00 bs
        else bs
  where
    integerToBytesBE x = B.pack $ reverse $ go x
      where
        go 0 = []
        go v = fromIntegral (v `rem` 256) : go (v `quot` 256)

encodeDER :: (Integer, Integer) -> B.ByteString
encodeDER (r, s) = C.runPut $ do
    let rDer = integerToDER r
        sDer = integerToDER s
        total = B.length rDer + B.length sDer + 4
    C.putWord8 0x30
    C.putWord8 (fromIntegral total)
    putIntDER rDer
    putIntDER sDer
  where
    putIntDER bs = do
        C.putWord8 0x02
        C.putWord8 (fromIntegral $ B.length bs)
        C.putByteString bs

decodeDER :: B.ByteString -> Either String (Integer, Integer)
decodeDER = C.runGet $ do
    tag <- C.getWord8
    if tag /= 0x30
        then fail "Invalid DER: expected 0x30"
        else return ()
    _len <- C.getWord8
    r <- getIntDER
    s <- getIntDER
    return $ (r, s)
  where
    getIntDER = do
        t <- C.getWord8
        if t /= 0x02
            then fail "Invalid DER: expected 0x02"
            else return ()
        l <- fromIntegral <$> C.getWord8
        bs <- C.getByteString l
        return $ bytesToInteger (B.dropWhile (== 0x00) bs)
