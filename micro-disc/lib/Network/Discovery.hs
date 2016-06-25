{-# LANGUAGE OverloadedStrings #-}
module Network.Discovery
    ( getOptions
    , startDiscoveryService
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Network.Nats ( Connection
                    , LoggerSpec (..)
                    , Settings (..)
                    , defaultSettings
                    , runNatsClient
                    , pubJson'
                    , subAsync'
                    , subAsyncJson'
                    )
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS

import Network.Discovery.Info (Info (..))
import Network.Discovery.Options (Options (..), getOptions)
import Network.Discovery.Procedures ( register
                                    , unregister
                                    , discover
                                    , ping
                                    , pingServices
                                    )
import Network.Discovery.Registry (empty)

startDiscoveryService :: Options -> IO ()
startDiscoveryService opts = do
    let uri      = BS.pack $ natsUri opts
        settings = defaultSettings { loggerSpec = toLoggerSpec opts }
    runNatsClient settings uri natsConnected `catch` onException

-- | Top level exception handler. Just print the content of the
-- exception.
onException :: SomeException -> IO ()
onException e = printf "Oops. Got '%s'\n" (show e)

natsConnected :: Connection -> IO ()
natsConnected conn = do
    -- Create the registry.
    registry <- newTVarIO empty

    -- Subscribe to service registrations.
    void $ subAsyncJson' conn "service.register.*" $ register conn registry

    -- Subscribe to service unregistrations.
    void $ subAsync' conn "service.unregister.*" $ unregister conn registry

    -- Subscribe to service discovery.
    void $ subAsyncJson' conn "service.discover.*" $ discover conn registry

    -- Subscribe to ping sent to itself.
    void $ subAsync' conn "micro-disc.ping" $ ping conn

    -- Register itself.
    let info = Info { service    = "micro-disc"
                    , version    = "0.0.1.0"
                    , interfaces = []
                    }
    pubJson' conn "service.register.micro-disc" info
    
    -- Every 5th second, ping all registered services.
    forever $ do
        threadDelay 5000000
        pingServices conn registry

-- | From options, create a 'LoggerSpec'.
toLoggerSpec :: Options -> LoggerSpec
toLoggerSpec opts =
    case logger opts of
        Just "stdout" -> StdoutLogger
        Just file     -> FileLogger file
        Nothing       -> NoLogger

