{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information provided by a service when registering itself.
module Network.Discovery.Info
    ( Info (..)
    , Interface (..)
    , Protocol (..)
    , Payload (..)
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
      -- ^ The protocol used to connect to the interface.
    , address  :: !IPAddress
      -- ^ The endpoint IP address for the interface.
    , port     :: !PortNum
      -- ^ The endport port number for the interface.
    , payload  :: !Payload
      -- ^ The type of payload transported over this interface.
    } deriving (Generic, Show)

data Protocol
    = SCTP
    | TCP
    | UDP
    | HTTP
    | HTTPS
    | Protocol !Text
    deriving Show

data Payload
    = HTML
    | JSON
    | XML
    | Payload !Text
    deriving Show

instance FromJSON Interface
instance ToJSON Interface

instance FromJSON Protocol where
    parseJSON (String s)
        | "SCTP"  == T.toUpper s = pure SCTP
        | "TCP"   == T.toUpper s = pure TCP
        | "UDP"   == T.toUpper s = pure UDP
        | "HTTP"  == T.toUpper s = pure HTTP
        | "HTTPS" == T.toUpper s = pure HTTPS
        | otherwise              = pure $ Protocol s
    parseJSON invalid    = typeMismatch "Protocol" invalid

instance ToJSON Protocol where
    toJSON SCTP         = String "SCTP"
    toJSON TCP          = String "TCP"
    toJSON UDP          = String "UDP"
    toJSON HTTP         = String "HTTP"
    toJSON HTTPS        = String "HTTPS"
    toJSON (Protocol x) = String x

instance FromJSON Payload where
    parseJSON (String s)
        | "HTML" == T.toUpper s = pure HTML
        | "JSON" ==
