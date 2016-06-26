{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Discovery.Procedures
    ( register
    , unregister
    , discover
    , ping
    , pingServices
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Network.Nats
import qualified Data.ByteString.Char8 as BS

import Network.Discovery.Context (Context (..))
import Network.Discovery.Info (Info)
import Network.Discovery.Options (Options (..))
import Network.Discovery.Registry ( Entry (..)
                                  , registerService
                                  , discoverService
                                  , removeService
                                  , removeServiceIf
                                  , toList
                                  )

-- | Register the service, where the service name is the last part of
-- the topic. If there are any pending discoveries to this service those
-- are published to their registered reply-to topics.
register :: Context -> JsonMsg Info -> IO ()
register Context {..} (JsonMsg topic _ _ (Just info)) = do
    let service = topic2Service topic
    pendings <- atomically $ registerService service info registry
    forM_ pendings $ \pending -> pubJson' conn pending info

-- JSON was unable to decode.
register _ (JsonMsg _ _ _ Nothing) = putStrLn "register: JSON error"

-- | Unregister a service. Notify supervision subscribers of it.
unregister :: Context -> NatsMsg -> IO ()
unregister Context {..} (NatsMsg topic _ _ _) = do
    let service = topic2Service topic
    atomically $ removeService service registry
    pub' conn ("service.supervise." `BS.append` service) "Unregistered"

-- | Procedure to discover a service. If the service is registered
-- an 'Info' record is published immediately. Otherwise the reply-to
-- topic is stored until the requested service is registered.
discover :: Context -> JsonMsg Info -> IO ()
discover Context {..} (JsonMsg topic _ (Just replyTo) _) = do
    let service = topic2Service topic
    mInfo <- atomically $ discoverService service replyTo registry
    maybe (return ()) (pubJson' conn replyTo) mInfo

-- No reply-to topic supplied. Cannot reply.
discover _ (JsonMsg _ _ Nothing _) = putStrLn "discover: no reply-to"

-- | Procedure of handling a ping message. Just reply back to the requester
-- with the provided payload. Nothing more, nothing less.
ping :: Context -> NatsMsg -> IO ()
ping Context {..} (NatsMsg _ _ (Just replyTo) payload) = 
    pub' conn replyTo payload

-- No reply-to topic supplied. Cannot reply.
ping _ (NatsMsg _ _ Nothing _) = putStrLn "ping: no reply-to"

-- | Ping all the registered services. Services that not reply will be
-- removed. There's a catch though, it is possible that the service have
-- re-registered during the ping period. The service is only removed if
-- it have the same generation count as when it was pinged. If the
-- service was removed, supervision subscribers of it is notified.
pingServices :: Context -> IO ()
pingServices Context {..} =
    mapM_ (forkIO . pingService)
        =<< filter isEntry <$> (atomically $ toList registry)
    where
      pingService :: (ByteString, Entry) -> IO ()
      pingService (service, Entry gen _) = do
          let wait = Sec $ waitTime options
          gotPong <- request conn (service `BS.append` ".ping") "" wait
          case gotPong of
              Nothing -> do
                  isRemoved <-
                    atomically $ removeServiceIf (== gen) service registry
                  when isRemoved $
                    pub' conn ("service.supervise." `BS.append` service)
                         "Not replying to ping"
              Just _  -> return ()

      pingService _ = undefined

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
