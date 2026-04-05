{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Connection
    ( withConnection
    , runMessageLoop
    ) where

import Network.Socket
    ( Socket, SockAddr(..), Family(AF_INET, AF_INET6)
    , SocketType(Stream), socket, connect, close, defaultProtocol
    )
import Network.Socket.ByteString (recv)
import Data.Serialize.Get        (runGetPartial, Result(..))
import qualified Data.ByteString as BS
import Control.Exception         (bracket)
import Data.Serialize (get)

import Bitcoin.Network.Message (Message)

withConnection :: SockAddr -> (Socket -> IO a) -> IO a
withConnection addr action = do
    putStrLn $ "[Connection] 연결 시도: " ++ show addr
    let family = case addr of
            SockAddrInet {}  -> AF_INET
            SockAddrInet6 {} -> AF_INET6
            _                -> AF_INET
    
    bracket (socket family Stream defaultProtocol) close $ \sock -> do
        connect sock addr
        putStrLn "[Connection] TCP 연결 성공"
        action sock

-- | 초기 버퍼를 받아 파싱을 시작하고, 루프 종료 시 '남은 버퍼'를 반환합니다.
runMessageLoop :: Socket -> BS.ByteString -> (Message -> IO Bool) -> IO (Either String BS.ByteString)
runMessageLoop sock initialBuffer onMessage = go (runGetPartial get initialBuffer)
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
            shouldContinue <- onMessage msg
            if shouldContinue
                then go (runGetPartial get rest)
                -- ★ 핵심: 루프를 끝낼 때 남은 찌꺼기 버퍼(rest)를 살려서 반환합니다!
                else return $ Right rest
