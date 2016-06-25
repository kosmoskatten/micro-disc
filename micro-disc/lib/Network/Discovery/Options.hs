module Network.Discovery.Options
    ( Options (..)
    , getOptions
    ) where

import Options.Applicative

-- | Command line options to the micro-disc program.
data Options = Options
    { natsUri  :: !String
      -- ^ The URI to the NATS server to connect to.
    , logger   :: !(Maybe FilePath)
      -- ^ Specification of logging.
    , pingFreq :: !Int
      -- ^ The frequency when micro-disc shall ping all its
      -- registered services. In seconds.
    , waitTime :: !Int
      -- ^ The wait time for a pinged service to reply until it's
      -- purged from the directory.
    } deriving Show

-- | Generate the 'Options' from the list of command line arguments.
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
            <*> option auto
                ( long "freq"
               <> short 'f'
               <> metavar "<PING FREQUENCY>"
               <> value 5
               <> help "Service ping frequency. In seconds (default: 5)"
                )
            <*> option auto
                ( long "wait"
               <> short 'w'
               <> metavar "<PING WAIT TIME>"
               <> value 2
               <> help "Service ping wait time. In seconds (default: 2)"
                )
