module Main
    ( main
    ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Discovery.InfoProps (encodeDecodeInfo)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "JSON encoding/decoding property tests"
        [ testProperty "Encode/decode of Info records"
                       encodeDecodeInfo
        ]
    ]
