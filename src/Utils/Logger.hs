module Utils.Logger
    ( logConsole
    , Direction(..)
    , writeBinary
    , readBinary
    ) where

import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | 콘솔에 진행 상황 메시지를 출력합니다.
logConsole :: String -> IO ()
logConsole = putStrLn

-- | 네트워크 데이터의 방향 (요청/응답)
data Direction = Request | Response
    deriving (Eq, Show)

-- | 방향을 파일명에 사용할 짧은 문자열(req/res)로 변환하는 헬퍼 함수
dirPrefix :: Direction -> String
dirPrefix Request  = "req"
dirPrefix Response = "res"

-- | 방향(요청/응답), 명령어, 타임스탬프를 조합하여 바이너리 파일을 저장합니다.
writeBinary :: Direction -> String -> BS.ByteString -> IO ()
writeBinary direction cmd bs = do
    ts <- round <$> getPOSIXTime :: IO Integer
    let filename = "data/" ++ dirPrefix direction ++ "_" ++ cmd ++ "_" ++ show ts ++ ".dat"
    putStrLn $ "[Dump] 바이너리 파일 저장 완료: " ++ filename
    BS.writeFile filename bs

-- | 저장된 바이너리(.dat) 파일을 다시 읽어옵니다. (추후 테스트 및 디버깅용)
readBinary :: FilePath -> IO BS.ByteString
readBinary path = do
    putStrLn $ "[Load] 바이너리 파일 읽기: " ++ path
    BS.readFile path
