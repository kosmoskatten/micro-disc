module Network.Discovery.Registry
    ( Registry
    , Entry (..)
    , empty
    , registerService
    , discoverService
    , removeService
    , removeServiceIf
    , toList
    ) where

import Control.Concurrent.STM (STM, TVar, modifyTVar, readTVar, writeTVar)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.HashMap.Lazy (HashMap)
import Network.Nats (Topic)

import qualified Data.HashMap.Lazy as HashMap

import Network.Discovery.Info (Info)

-- | Convenience data type to name a Service.
type Service = ByteString

-- | Convenience data type for our registry.
type Registry = HashMap ByteString Entry

-- | An entry in the registry.
data Entry
    = Entry !Int !Info
      -- ^ An entry is a generation count and an 'Info' record.
    | Pending ![Topic]
      -- ^ A pending entry hold a list of reply-to topics interested
      -- in discovering the service.
    deriving (Eq, Show)

-- | Create an empty registry.
empty :: Registry
empty = HashMap.empty

-- | Register a service together with its 'Info' record. If the service
-- is present since before the new registration is made with an
-- incremented generation count. If requests have been made to discover
-- the service before it's registered there might be a list of
-- reply-to topics at the return of the registration.
registerService :: Service -> Info -> TVar Registry -> STM [Topic]
registerService service info reg = do
    reg' <- readTVar reg
    case HashMap.lookup service reg' of
        Just (Entry gen _) -> do
            writeTVar reg $ HashMap.insert service 
                                    (Entry (gen + 1) info) reg'
            return []

        Just (Pending xs)  -> do
            writeTVar reg $ HashMap.insert service (Entry 1 info) reg'
            return xs

        Nothing            -> do
            writeTVar reg $ HashMap.insert service (Entry 1 info) reg'
            return []

-- | Discover a service. If the service is registered its 'Info' record
-- is returned. If the service not is registered the provided reply-to
-- topic is stored and will become notified once the service is registered.
discoverService :: Service -> Topic -> TVar Registry -> STM (Maybe Info)
discoverService service replyTo reg = do
    reg' <- readTVar reg
    case HashMap.lookup service reg' of
        Just (Entry _ info) -> return (Just info)

        Just (Pending xs)   -> do
            writeTVar reg $ HashMap.insert service
                                    (Pending (replyTo:xs)) reg'
            return Nothing

        Nothing             -> do
            writeTVar reg $ HashMap.insert service (Pending [replyTo]) reg'
            return Nothing

-- | Remove a service.
removeService :: Service -> TVar Registry -> STM ()
removeService service reg = modifyTVar reg $ HashMap.delete service

-- | Remove a service if the predicate evaluates to True. The
-- predicate is given the generation count of the entry to remove. If
-- no entry is found the function will return False, if an entry is found
-- the value of the predicate is returned.
removeServiceIf :: (Int -> Bool) -> Service -> TVar Registry -> STM Bool
removeServiceIf g service reg = do
    reg' <- readTVar reg
    case HashMap.lookup service reg' of
        Just (Entry gen _) -> do
            let result = g gen
            when result $ writeTVar reg $ HashMap.delete service reg'
            return result
        _                  -> return False

-- | Export the registry to a list.
toList :: TVar Registry -> STM [(ByteString, Entry)]
toList tvar = HashMap.toList <$> readTVar tvar
