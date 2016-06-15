module Main
    ( main
    ) where

import Options.Applicative

data Options = Options
    { natsUri :: !String
    , logger  :: !(Maybe String)
    }

main :: IO ()
main = putStrLn "kk"
