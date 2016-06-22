module Main
    ( main
    ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Network.Discovery.RegistryTests ( registerOneService
                                       , reRegisterOneService
                                       , discoverNonExistService
                                       , discoverExistingService
                                       )

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Registry unit tests"
        [ testCase "Register one service" registerOneService
        , testCase "Re-register one service" reRegisterOneService
        , testCase "Discover a non-exist service" discoverNonExistService
        , testCase "Discover an existing service" discoverExistingService
        ]
    ]
