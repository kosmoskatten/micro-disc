{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, forever)
import Data.ByteString.Char8 (ByteString)
import Data.String.Conv (toS)
import Network.Discovery.Info (Info (..))
import Network.Nats ( Connection
                    , NatsURI
                    , JsonMsg (..)
                    , NatsMsg (..)
                    , Timeout (..)
                    , runNatsClient
                    , defaultSettings
                    , pub'
                    , pubJson'
                    , requestJSON
                    , subAsync'
                    )
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    args <- map BS.pack <$> getArgs
    case args of
        x:y:ys -> initService x y ys
        _ -> putStrLn "Usage: example-service <nats uri> <name> [services]"

initService :: NatsURI -> ByteString -> [ByteString] -> IO ()
initService natsUri name services =
    runNatsClient defaultSettings natsUri $ natsConnected name services

natsConnected :: ByteString -> [ByteString] -> Connection -> IO ()
natsConnected name services conn = do
    -- First register yourselves.
    let info = Info { service = toS name
                    , version = "0.1.0"
                    , interfaces = []
                    }
    pubJson' conn ("service.register." `BS.append` name) info

    -- Subscribe to pings.
    void $ subAsync' conn (name `BS.append` ".ping") $ handlePing conn

    -- Discover and supervise the services.
    mapM_ (discoverAndSupervise conn) services

    -- Just keep main thread alive.
    forever $ threadDelay 1000000

discoverAndSupervise :: Connection -> ByteString -> IO ()
discoverAndSupervise conn name = do
    BS.putStrLn $ "Waiting to discover " `BS.append` name
    let topic = "service.discover." `BS.append` name
    Just (JsonMsg _ _ _ info) <- requestJSON conn topic emptyJSON Infinity
    BS.putStrLn $ "Done! Got " `BS.append` toS (show (info :: Maybe Info))

handlePing :: Connection -> NatsMsg -> IO ()
handlePing conn (NatsMsg _ _ (Just replyTo) payload) = do
    pub' conn replyTo payload
    putStrLn "Handled ping"
handlePing _ _ = putStrLn "Ping without anywhere to pong"

emptyJSON :: [Int]
emptyJSON = []
