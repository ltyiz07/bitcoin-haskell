{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Network.Discovery
    ( getPeerIPs
    , defaultDnsSeeds
    , showSockAddr
    ) where

import Network.Socket
    ( HostName
    , ServiceName
    , SockAddr
    , AddrInfo(addrSocketType, addrAddress)
    , SocketType(Stream)
    , defaultHints
    , getAddrInfo
    )
import Control.Exception (catch, SomeException)
import Data.List (nub)

showSockAddr :: SockAddr -> String
showSockAddr = showSockAddr

-- 비트코인 코어에 하드코딩된 메인넷 DNS 시드 목록입니다.
defaultDnsSeeds :: [(HostName,  ServiceName)]
defaultDnsSeeds =
    [ ("seed.bitcoin.sipa.be", "8333")
    , ("dnsseed.bluematt.me", "8333")
    , ("dnsseed.bitcoin.dashjr.org", "8333")
    , ("seed.bitcoinstats.com", "8333")
    , ("seed.bitcoin.jonasschnelli.ch", "8333")
    , ("seed.btc.petertodd.org", "8333")
    ]

-- | 특정 DNS 시드와 포트를 쿼리하여 접속 가능한 비트코인 노드의 주소(IP, Port) 목록을 반환합니다.
getPeerIPs :: HostName -> ServiceName -> IO [SockAddr]
getPeerIPs seedHost port = do
    putStrLn $ "[Discovery] DNS 시드 쿼리 중... : " ++ seedHost ++ ":" ++ port
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) (Just seedHost) (Just port) `catch` handleError
    -- 중복을 제거(nub)하고 SockAddr 타입만 추출합니다.
    let addrs = nub $ map addrAddress addrInfos
    putStrLn $ "[Discovery] " ++ show (length addrs) ++ " 개의 노드 주소를 찾았습니다."
    return addrs

  where
    handleError :: SomeException -> IO [AddrInfo]
    handleError err = do
        putStrLn $ "[Discovery] DNS 쿼리 실패 (" ++ seedHost ++ ":" ++ port ++ "): " ++ show err
        return []
