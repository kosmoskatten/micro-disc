module Main
    ( main
    ) where

import Data.ByteString.Char8 (ByteString)
import Network.Discovery.Info (Info (..))
import Network.Nats
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    args <- map BS.pack <$> getArgs
    case args of
        []   -> printf "Usage: example-service <own name> [others]"
        x:xs -> initService x xs

initService :: ByteString -> [ByteString] -> IO ()
initService = undefined
