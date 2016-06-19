module Network.Discovery.Procedures
    ( register
    , discover
    , ping
    , pingServices
    ) where

import Control.Concurrent.STM (TVar)
import Network.Nats

import Network.Discovery.Info (Info (..))
import Network.Discovery.Registry (Registry)

register :: Connection -> TVar Registry -> JsonMsg Info -> IO ()
register = undefined

discover :: Connection -> TVar Registry -> JsonMsg Info -> IO ()
discover = undefined

ping :: Connection -> NatsMsg -> IO ()
ping = undefined

pingServices :: Connection -> TVar Registry -> IO ()
pingServices = undefined

