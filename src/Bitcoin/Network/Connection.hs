{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( NodeConnection
    , ConnectionError(..)
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
import Control.Exception         (bracket, catch, SomeException)
import Control.Monad.Except      (ExceptT(..), throwError, runExceptT )

-- ---------------------------------------------------------------------------
-- 타입 정의
-- ---------------------------------------------------------------------------

-- | 연결 및 통신 중 발생할 수 있는 에러를 명시적으로 표현합니다.
-- String 대신 구체적인 타입을 사용하면 호출부에서 패턴 매칭이 가능합니다.
data ConnectionError
    = ConnectFailed   String  -- ^ TCP 연결 자체 실패
    | ParseError      String  -- ^ 수신 메시지 역직렬화 실패
    | ConnectionClosed        -- ^ 상대방이 소켓을 닫음
    deriving (Show, Eq)

-- | 소켓과 수신 버퍼(Leftover)의 상태를 하나로 묶은 커스텀 데이터 타입.
-- 생성자를 외부에 노출하지 않아 직접 조작을 방지합니다.
data NodeConnection = NodeConnection Socket (IORef BS.ByteString)

-- | 수신 버퍼 크기 (bytes)
recvBufferSize :: Int
recvBufferSize = 32768

-- ---------------------------------------------------------------------------
-- 공개 API
-- ---------------------------------------------------------------------------

-- | 소켓을 열고 버퍼를 초기화한 뒤 안전하게 닫아주는 함수.
-- 연결 실패 시 예외를 던지는 대신 Left ConnectFailed 를 반환합니다.
withConnection
    :: SockAddr
    -> (NodeConnection -> ExceptT ConnectionError IO a)
    -> ExceptT ConnectionError IO a
withConnection addr action = ExceptT $ do
    putStrLn $ "[Connection] 연결 시도: " ++ show addr
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET

    result <- bracket (socket family Stream defaultProtocol) close $ \sock ->
        (do
            connect sock addr
            putStrLn "[Connection] TCP 연결 성공"
            bufferRef <- newIORef BS.empty
            runExceptT $ action (NodeConnection sock bufferRef)
        ) `catch` \(e :: SomeException) ->
            return $ Left $ ConnectFailed (show e)

    return result

-- | 직렬화 가능한 메시지를 네트워크로 전송합니다.
sendMessage :: Serialize a => NodeConnection -> a -> ExceptT ConnectionError IO ()
sendMessage (NodeConnection sock _) msg = ExceptT $
    (sendAll sock (runPut $ put msg) >> return (Right ()))
    `catch` \(e :: SomeException) ->
        return $ Left $ ConnectFailed ("전송 실패: " ++ show e)

-- | 네트워크에서 완전한 메시지 1개가 조립될 때까지 대기 후 반환합니다.
-- 내부적으로 찌꺼기 버퍼(IORef)를 관리하므로 호출부에서 버퍼를 신경 쓸 필요가 없습니다.
recvMessage :: Serialize a => NodeConnection -> ExceptT ConnectionError IO a
recvMessage (NodeConnection sock bufferRef) = ExceptT $ do
    initialBuf <- readIORef bufferRef
    go (runGetPartial get initialBuf)
  where
    go parseState = case parseState of
        Fail err _ ->
            return $ Left $ ParseError err

        Partial feed -> do
            chunk <- recv sock recvBufferSize
            if BS.null chunk
                then return $ Left ConnectionClosed
                else go (feed chunk)

        Done msg rest -> do
            -- 메시지 1개 완성 후 남은 바이트를 버퍼에 보존합니다.
            writeIORef bufferRef rest
            return $ Right msg
