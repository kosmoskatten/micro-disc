module Network.Discovery.Registry
    ( Registry
    , Entry (..)
    , empty
    , registerService
    , discoverService
    , toList
    ) where

import Control.Concurrent.STM (STM, TVar, readTVar)
import Data.ByteString (ByteString)
import Data.HashMap.Lazy (HashMap)
import Network.Nats (Topic)

import qualified Data.HashMap.Lazy as HashMap

import Network.Discovery.Info (Info)

type Service = ByteString
type Registry = HashMap ByteString Entry

data Entry
    = Entry !Int !Info
    | Pending ![Topic]
    deriving (Eq, Show)

empty :: Registry
empty = HashMap.empty

registerService :: Service -> Info -> TVar Registry -> STM [Topic]
registerService = undefined

discoverService :: Service -> Topic -> TVar Registry -> STM (Maybe Info)
discoverService = undefined

toList :: TVar Registry -> STM [(ByteString, Entry)]
toList tvar = HashMap.toList <$> readTVar tvar
