module Main
    ( main
    ) where

import Network.Discovery (getOptions, startDiscoveryService)

main :: IO ()
main = startDiscoveryService =<< getOptions
