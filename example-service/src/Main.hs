{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Example program to show how service discovery and service supervision
-- can be made using micro-disc and NATS.
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

-- | A collection of data used during service runtime.
data Context = Context
    { conn :: !Connection
      -- ^ Connection to NATS.
    , out  :: !Out
      -- ^ A handle on which synchronized printing to stdout can
      -- be made.
    , kill :: !KillSwitch
      -- ^ A killswitch, which secondary threads can use to terminate
      -- the service.
    }

-- | Commands sent to the killswitch.
data Heartbeat = Heart | Die
    deriving (Eq, Show)

-- | Convenience type alias for the synchronized stdout channel.
type Out = MVar ()

-- | Convencience type alias for the killswitch 'MVar'.
type KillSwitch = MVar Heartbeat

main :: IO ()
main = do
    args <- map BS.pack <$> getArgs
    case args of
        nats:name:services -> initService nats name services
        _                  ->
            putStrLn "Usage: example-service <nats uri> <name> [services]"

initService :: NatsURI -> ByteString -> [ByteString] -> IO ()
initService natsUri name services =
    runNatsClient defaultSettings natsUri $ natsConnected name services

natsConnected :: ByteString -> [ByteString] -> Connection -> IO ()
natsConnected name services conn' = do
    BS.putStrLn $ name `BS.append` " is connected to NATS."

    -- Create a runtime context for this service.
    context <- Context conn' <$> newMVar ()
                             <*> newKillSwitch

    -- First register yourselves.
    let info = Info { service = toS name
                    , version = "0.1.0"
                    , interfaces = []
                    }

    pubJson' conn' (serviceRegister name) info

    -- Subscribe to pings from the service directory (and others who likes
    -- to speak with you).
    void $ subAsync' conn' (selfPing name) $ handlePing context

    -- Discover and supervise the services specified at the
    -- command line.
    mapM_ (discoverAndSupervise context) services

    -- Wait until a 'Die' message is received, or until someone
    -- types ^C.
    waitUntilKilled context
    where
      waitUntilKilled :: Context -> IO ()
      waitUntilKilled context@Context {..} = do
          h <- takeMVar kill
          if h == Heart then waitUntilKilled context
                        else sPutStrLn out "Terminating ..."

-- | Discover and supervise a service with the given name.
discoverAndSupervise :: Context -> ByteString -> IO ()
discoverAndSupervise context@Context {..} name = do
    sPutStrLn out $ "Waiting to discover " `BS.append` name
    Just (JsonMsg _ _ _ info) <-
        requestJSON conn (serviceDiscover name) emptyJSON Infinity
    sPutStrLn out $ "Done! Got " `BS.append` toS (show (info :: Maybe Info))

    void $ subAsync' conn (serviceSupervise name) $
        handleSupervision context name

-- | Handle pings. Just reply with the payload sent.
handlePing :: Context -> NatsMsg -> IO ()
handlePing Context {..} (NatsMsg _ _ (Just replyTo) payload) = do
    pub' conn replyTo payload
    sPutStrLn out "*PING => PONG*"
handlePing Context {..} _ = sPutStrLn out "Ping without anywhere to pong"

-- | Handle the case where a supervised service has gone down. Just
-- print info and send kill signal to main thread.
handleSupervision :: Context -> ByteString -> NatsMsg -> IO ()
handleSupervision Context {..} name _ = do
    sPutStrLn out $ name `BS.append` " has died."
    putMVar kill Die

-- | Helper function to produce the topic of ping to self.
selfPing :: ByteString -> ByteString
selfPing name = name `BS.append` ".ping"

-- | Helper functions to produce topic strings.
serviceRegister, serviceDiscover, serviceSupervise :: ByteString
                                                   -> ByteString
serviceRegister = BS.append "service.register."
serviceDiscover = BS.append "service.discover."
serviceSupervise = BS.append "service.supervise."

-- | Just to produce some empty JSON data. Should perhaps be available
-- in nats-client?
emptyJSON :: [Int]
emptyJSON = []

-- | Create a kill switch, a way for a secondary thread to stop the
-- program. To prevent the GHC runtime to detect "blocking forever on
-- MVar" a helper thread is producing a 'Heart' value each second.
newKillSwitch :: IO KillSwitch
newKillSwitch = do
    kill <- newEmptyMVar
    void $ forkIO $
        forever $ do
            threadDelay 1000000
            putMVar kill Heart
    return kill

-- | The example-service is a multithreaded program. To avoid stuff
-- written to stdout become gibberish a simple 'MVar' synchronization
-- is made. Otherwise it works like 'BS.putStrLn'.
sPutStrLn :: Out -> ByteString -> IO ()
sPutStrLn out str = withMVar out $ const (BS.putStrLn str)
