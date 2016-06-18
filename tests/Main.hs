module Main
    ( main
    ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Discovery.InfoProps (encodeDecodeInfo)
import Network.Discovery.RegistryTests ( registerOneService
                                       , reRegisterOneService
                                       , discoverNonExistService
                                       , discoverExistingService
                                       )

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "JSON encoding/decoding property tests"
        [ testProperty "Encode/decode of Info records" encodeDecodeInfo
        ]
    , testGroup "Registry unit tests"
        [ testCase "Register one service" registerOneService
        , testCase "Re-register one service" reRegisterOneService
        , testCase "Discover a non-exist service" discoverNonExistService
        , testCase "Discover an existing service" discoverExistingService
        ]
    ]
