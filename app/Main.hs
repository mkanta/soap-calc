module Main where

import Lib
import CmdLine

-- This accepts --fatspecs "oiltype:value, oiltype:value,..." options ie
-- the fatspecs option expects a comma-separated list of oiltype:value fields
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

-- TODO: need a function like greet that takes the FatOpts, starts a database
-- query to get the soap values and complains if some are not in the
-- database. Need a datatype consisting of a FatSpec and corresponding
-- lye or potash factors.
