{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information provided by a service when registering itself.
module Network.Discovery.Info
    ( Info (..)
    , Interface (..)
    , Protocol (..)
    , IPAddress
    , PortNum
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T

-- | Type alias for IP addresses.
type IPAddress = Text

-- | Type alias for port numbers.
type PortNum = Int

data Info = Info

data Interface = Interface
    { role     :: !Text
      -- ^ The role for the interface. Some symbolic role description
      -- selected by the owner of the interface.
    , protocol :: !Protocol
    , address  :: !IPAddress
    , port     :: !(Maybe PortNum)
    } deriving (Generic, Show)

data Protocol
    = SCTP
    | TCP
    | UDP
    deriving Show

instance FromJSON Interface
instance ToJSON Interface

instance FromJSON Protocol where
    parseJSON val@(String s)
        | "SCTP" == T.toUpper s = pure SCTP
        | "TCP"  == T.toUpper s = pure TCP
        | "UDP"  == T.toUpper s = pure UDP
        | otherwise             = typeMismatch "SCTP, TCP or UDP" val
    parseJSON invalid    = typeMismatch "Protocol" invalid

instance ToJSON Protocol where
    toJSON SCTP = String "SCTP"
    toJSON TCP  = String "TCP"
    toJSON UDP  = String "UDP"
