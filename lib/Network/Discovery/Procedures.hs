{-# LANGUAGE OverloadedStrings #-}
module Network.Discovery.Procedures
    ( register
    , discover
    , ping
    , pingServices
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Network.Nats
import qualified Data.ByteString.Char8 as BS

import Network.Discovery.Info (Info)
import Network.Discovery.Registry ( Registry
                                  , Entry (..)
                                  , registerService
                                  , discoverService
                                  , toList
                                  )

-- | Register the service, where the service name is the last part of
-- the topic. If there are any pending discoveries to this service those
-- are published to their registered reply-to topics.
register :: Connection -> TVar Registry -> JsonMsg Info -> IO ()
register conn registry (JsonMsg topic _ _ (Just info)) = do
    let service = topic2Service topic
    pendings <- atomically $ registerService service info registry
    forM_ pendings $ \pending -> pubJson' conn pending info

-- JSON was unable to decode.
register _ _ (JsonMsg _ _ _ Nothing) = putStrLn "register: JSON error"

-- | Procedure to discover a service. If the service is registered
-- an 'Info' record is published immediately. Otherwise the reply-to
-- topic is stored until the requested service is registered.
discover :: Connection -> TVar Registry -> JsonMsg Info -> IO ()
discover conn registry (JsonMsg topic _ (Just replyTo) _) = do
    let service = topic2Service topic
    mInfo <- atomically $ discoverService service replyTo registry
    maybe (return ()) (pubJson' conn replyTo) mInfo

-- No reply-to topic supplied. Cannot reply.
discover _ _ (JsonMsg _ _ Nothing _) = putStrLn "discover: no reply-to"

-- | Procedure of handling a ping message. Just reply back to the requester
-- with the provided payload. Nothing more, nothing less.
ping :: Connection -> NatsMsg -> IO ()
ping conn (NatsMsg _ _ (Just replyTo) payload) = pub' conn replyTo payload

-- No reply-to topic supplied. Cannot reply.
ping _ (NatsMsg _ _ Nothing _) = putStrLn "ping: no reply-to"

pingServices :: Connection -> TVar Registry -> IO ()
pingServices conn registry = do
    services <- filter isEntry <$> (atomically $ toList registry)
    forM_ services $ \(service, entry) ->
        forkIO $ do
            mPong <- request conn (service `BS.append` ".ping") "" (Sec 3)
            case mPong of
                Just _ -> putStrLn "PONG"
                Nothing -> putStrLn "NO PONG"

-- | Assume a topic string like: "service.register.foo" or
-- service.discover.foo". The substring "foo" will be returned as the
-- service's name.
topic2Service :: ByteString -> ByteString
topic2Service topic =
    let [_, _, service] = BS.split '.' topic
    in service

isEntry :: (ByteString, Entry) -> Bool
isEntry (_, Entry _ _) = True
isEntry _              = False

