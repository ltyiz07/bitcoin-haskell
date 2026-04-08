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
import Control.Monad.Except      (ExceptT(..), runExceptT )

import Bitcoin.Network.Message.NetworkMagic (Network(..), magicBytes)


data ConnectionError
    = ConnectFailed   String  -- TCP 연결 자체 실패
    | ParseError      String  -- 수신 메시지 역직렬화 실패
    | ConnectionClosed        -- 상대방이 소켓을 닫음
    deriving (Show, Eq)

data NodeConnection = NodeConnection Socket (IORef BS.ByteString)

recvBufferSize :: Int
recvBufferSize = 32768

withConnection :: SockAddr
               -> (NodeConnection -> ExceptT ConnectionError IO a)
               -> ExceptT ConnectionError IO a
withConnection addr action = ExceptT $ do
    putStrLn $ "[Connection] 연결 시도: " ++ show addr
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET
    (bracket (socket family Stream defaultProtocol) close $ \sock ->
        do
            connect sock addr
            putStrLn "[Connection] TCP 연결 성공"
            bufferRef <- newIORef BS.empty
            runExceptT $ action (NodeConnection sock bufferRef)
        ) `catch` \(e :: SomeException) ->
            return $ Left $ ConnectFailed (show e)

sendMessage :: Serialize a => NodeConnection -> a -> ExceptT ConnectionError IO ()
sendMessage (NodeConnection sock _) msg = ExceptT $
    (sendAll sock (runPut $ put msg) >> return (Right ()))
    `catch` \(e :: SomeException) ->
        return $ Left $ ConnectFailed ("전송 실패: " ++ show e)

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
            writeIORef bufferRef rest
            return $ Right msg
