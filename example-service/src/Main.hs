{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar ( MVar
                               , newMVar
                               , newEmptyMVar
                               , putMVar
                               , takeMVar
                               , withMVar
                               )
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

data Heartbeat = Heart | Die
    deriving (Eq, Show)

type Out = MVar ()
type Kill = MVar Heartbeat

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
    -- Create a simple synchronized stdout writer.
    out <- newSyncOutput
    sPutStrLn out $ name `BS.append` " is connected to NATS."

    -- Create a 'kill switch' where supervision threads can signal to
    -- main thread to exit.
    kill <- newKillSwitch

    -- First register yourselves.
    let info = Info { service = toS name
                    , version = "0.1.0"
                    , interfaces = []
                    }

    pubJson' conn ("service.register." `BS.append` name) info

    -- Subscribe to pings.
    void $ subAsync' conn (name `BS.append` ".ping") $ handlePing out conn

    -- Discover and supervise the services.
    mapM_ (discoverAndSupervise out kill conn) services

    waitUntilKilled kill
    where
      waitUntilKilled kill = do
          h <- takeMVar kill
          if h == Heart then waitUntilKilled kill
                        else putStrLn "Terminating ..."

discoverAndSupervise :: Out -> Kill -> Connection -> ByteString -> IO ()
discoverAndSupervise out kill conn name = do
    sPutStrLn out $ "Waiting to discover " `BS.append` name
    let topic = "service.discover." `BS.append` name
    Just (JsonMsg _ _ _ info) <- requestJSON conn topic emptyJSON Infinity
    sPutStrLn out $ "Done! Got " `BS.append` toS (show (info :: Maybe Info))
    void $ subAsync' conn ("service.supervise." `BS.append` name) $
                     handleSupervision out kill name

handlePing :: Out -> Connection -> NatsMsg -> IO ()
handlePing out conn (NatsMsg _ _ (Just replyTo) payload) = do
    pub' conn replyTo payload
    sPutStrLn out "Handled ping"
handlePing out _ _ = sPutStrLn out "Ping without anywhere to pong"

handleSupervision :: Out -> Kill -> ByteString -> NatsMsg -> IO ()
handleSupervision out kill name _ = do
    sPutStrLn out $ name `BS.append` " has died."
    putMVar kill Die

emptyJSON :: [Int]
emptyJSON = []

newSyncOutput :: IO Out
newSyncOutput = newMVar ()

newKillSwitch :: IO Kill
newKillSwitch = do
    kill <- newEmptyMVar
    void $ forkIO $
        forever $ do
            threadDelay 1000000
            putMVar kill Heart
    return kill

sPutStrLn :: Out -> ByteString -> IO ()
sPutStrLn sync out = withMVar sync $ const (BS.putStrLn out)
