module Bitcoin.Network.Message.Payload.GetHeaders
    ( GetHeaders(..)
    ) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Control.Monad (replicateM)
import Data.Serialize
    ( Serialize(..)
    , putWord32le
    , putByteString
    , getWord32le
    , getByteString
    )

import Bitcoin.VarInt (VarInt(..))

-- | GetHeaders 메시지의 페이로드 구조체
-- 동기화를 위해 현재 노드가 알고 있는 최신 블록 해시들(locators)을 상대방에게 알려주고,
-- 상대방은 이 해시들을 바탕으로 우리가 없는 블록 헤더들을 전송해 줍니다.
data GetHeaders = GetHeaders
    { version  :: Word32            -- ^ 프로토콜 버전 (예: 70015)
    , locators :: [BS.ByteString]   -- ^ 블록 로케이터 해시 목록 (각 32 바이트, Little-endian)
    , hashStop :: BS.ByteString     -- ^ 중단할 블록 해시 (끝까지 받으려면 32바이트를 0으로 채움)
    } deriving (Show, Eq)

instance Serialize GetHeaders where
    put gh = do
        putWord32le gh.version
        -- 로케이터 해시의 개수를 VarInt로 직렬화하여 먼저 넣습니다.
        put $ VarInt (fromIntegral $ length gh.locators)
        -- 해시 리스트를 순회하며 32바이트씩 직렬화합니다.
        mapM_ putByteString gh.locators
        -- 마지막으로 중단점 해시를 넣습니다.
        putByteString gh.hashStop
    get = do
        ver <- getWord32le
        -- 해시 개수를 VarInt로 읽어옵니다.
        VarInt count <- get
        -- 알아낸 개수만큼 32바이트씩 읽어와서 리스트로 만듭니다.
        locs <- replicateM (fromIntegral count) (getByteString 32)
        stop <- getByteString 32
        return $ GetHeaders ver locs stop
