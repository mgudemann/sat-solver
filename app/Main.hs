module Main where

import           CDCL.CDCLFilereader (readCdclFile)
import           Options.Applicative

main :: IO ()
main = input =<< execParser options
  where
    options = info (inputParse <**> helper)
      ( fullDesc
     <> progDesc "Print a CDCL Result for TARGETFILE"
     <> header "Starting CDCL-SAT-Solver via Commandline " )

input :: CDCLInput -> IO ()
input (CDCLInput target v False False) = readCdclFile target v False False
input (CDCLInput target v True True)   = readCdclFile target v True True
input (CDCLInput target v False True)  = readCdclFile target v False True
input (CDCLInput target v True False)  = readCdclFile target v True False

data CDCLInput = CDCLInput
  { file  :: String
  , optV  :: Bool
  , opts  :: Bool
  , optsF :: Bool
  }

inputParse :: Parser CDCLInput
inputParse = CDCLInput
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "TARGETFILE"
         <> help "Target cnf-file which will be analysed from SAT-Solver" )
      <*> switch
          ( long "valuation"
         <> short 'v'
         <> help "Showing valuation of variables in case of SAT")
      <*> switch
          ( long "stats"
         <> short 's'
         <> help "Showing additional statistics")
      <*> switch
          ( long "full stats"
         <> short 'f'
         <> help "Showing all statistics")
