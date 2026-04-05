{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( NodeConnection
    , withConnection
    , sendMessage
    , recvMessage
    ) where

import Network.Socket
    ( Socket, SockAddr(..), Family(AF_INET, AF_INET6)
    , SocketType(Stream), socket, connect, close, defaultProtocol
    )
import Network.Socket.ByteString (recv, sendAll)
import Data.Serialize            (Serialize, put, get, runPut)
import Data.Serialize.Get        (runGetPartial, Result(..))
import qualified Data.ByteString as BS
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Control.Exception         (bracket)

-- | 소켓과 수신 버퍼(Leftover)의 상태를 하나로 묶은 커스텀 데이터 타입
data NodeConnection = NodeConnection Socket (IORef BS.ByteString)

-- | 소켓을 열고 버퍼를 초기화한 뒤 안전하게 닫아주는 함수
withConnection :: SockAddr -> (NodeConnection -> IO a) -> IO a
withConnection addr action = do
    putStrLn $ "[Connection] 연결 시도: " ++ show addr
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET
            
    bracket (socket family Stream defaultProtocol) close $ \sock -> do
        connect sock addr
        putStrLn "[Connection] TCP 연결 성공"
        
        -- 빈 바이트스트림으로 수신 버퍼(상태) 생성
        bufferRef <- newIORef BS.empty
        action (NodeConnection sock bufferRef)

-- | 직렬화(Serialize) 가능한 어떤 메시지든 네트워크로 전송합니다.
sendMessage :: Serialize a => NodeConnection -> a -> IO ()
sendMessage (NodeConnection sock _) msg = 
    sendAll sock (runPut $ put msg)

-- | 네트워크에서 완벽한 1개의 메시지가 조립될 때까지 대기하고 반환합니다.
-- 내부적으로 찌꺼기 버퍼(IORef)를 관리하여 외부에서는 버퍼에 신경 쓸 필요가 없습니다.
recvMessage :: Serialize a => NodeConnection -> IO (Either String a)
recvMessage (NodeConnection sock bufferRef) = do
    initialBuf <- readIORef bufferRef
    go (runGetPartial get initialBuf)
  where
    go parseState = case parseState of
        Fail err _ -> 
            return $ Left $ "파싱 에러: " ++ err
            
        Partial feed -> do
            chunk <- recv sock 32768
            if BS.null chunk
                then return $ Left "연결 끊김 (상대방이 소켓을 닫음)"
                else go (feed chunk)
                
        Done msg rest -> do
            -- ★ 1개의 메시지를 완성했다면, 남은 찌꺼기(rest)를 버퍼에 안전하게 보관!
            writeIORef bufferRef rest
            return $ Right msg
