{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Discovery.InfoProps
    ( encodeDecodeInfo
    ) where

import Data.Aeson (encode, decode)
import Data.Text (Text)
import Test.QuickCheck

import qualified Data.Text as T

import Network.Discovery.Info

instance Arbitrary Protocol where
    arbitrary = oneof [ pure SCTP
                      , pure TCP
                      , pure UDP
                      , pure HTTP
                      , pure HTTPS
                      , Protocol <$> arbitraryText
                      ]

instance Arbitrary Payload where
    arbitrary = oneof [ pure HTML
                      , pure JSON
                      , pure XML
                      , Payload <$> arbitraryText
                      ]

instance Arbitrary Interface where
    arbitrary = 
        Interface <$> arbitraryText
                  <*> arbitrary
                  <*> arbitraryText
                  <*> arbitrary
                  <*> arbitrary

instance Arbitrary Info where
    arbitrary =
        Info <$> arbitraryText
             <*> arbitraryText
             <*> arbitrary

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

encodeDecodeInfo :: Info -> Bool
encodeDecodeInfo info = maybe False (info ==) $ decode (encode info) 
