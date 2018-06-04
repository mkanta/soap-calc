module Main where

import Lib
import CmdLine

main :: IO ()
main =
  greet =<< execParser opts
  where
    opts = info (fatopts <**> helper)
      ( fullDesc
     <> progDesc "Print a fat specification"
     <> header "fat-opts - a test for optparse-applicative" )

-- The function that does something with the value returned from the parser.
greet :: FatOpts -> IO ()
greet = putStrLn . show
