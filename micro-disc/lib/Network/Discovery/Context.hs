module Network.Discovery.Context
    ( Context (..)
    ) where

import Network.Discovery.Options (Options)
import Network.Discovery.Registry (Registry)

import Control.Concurrent.STM (TVar)
import Network.Nats (Connection)

-- | Collection of stuff for the execution of micro-disc.
data Context = Context
    { conn     :: !Connection
      -- ^ The connection to NATS.
    , options  :: !Options
      -- ^ The command line options.
    , registry :: !(TVar Registry)
      -- ^ The registry, wrapped in a TVar.
    }
