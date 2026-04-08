module Bitcoin.Network.Message.Header
    ( MessageHeader(..)
    ) where

import Data.ByteString as BS

import Bitcoin.Network.Message.NetworkMagic (Network(..))


data MessageHeader = MessageHeader
    { network :: Network
    , command :: BS.ByteString
    } deriving (Show, Eq)

