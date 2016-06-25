module Network.Discovery.Options
    ( Options (..)
    , getOptions
    ) where

import Options.Applicative

data Options = Options
    { natsUri :: !String
    , logger  :: !(Maybe FilePath)
    } deriving Show

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options = info (helper <*> parser)
               ( fullDesc
              <> progDesc "Start the micro-disc service"
              <> header "Micro-service, service discovery"
               )

parser :: Parser Options
parser =
    Options <$> strOption
                ( long "nats"
               <> short 'n'
               <> metavar "<NATS URI>"
               <> help "NATS URI for connecting to NATS server"
                )
            <*> (optional $ strOption
                ( long "logger"
               <> short 'l'
               <> metavar "<LOGGER SPEC>"
               <> help "Logger spec, 'stdout' or filename"
                ))


