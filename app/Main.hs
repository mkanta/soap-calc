module Main where

import Lib
import CmdLine
import Gui

-- TODO: this should runGui only if there is a -i, --interactive flag set?
--       and then initialise runGui with fat options if any?
--       or when there are no options on the command line at all.
main :: IO ()
main = runGui

{-- This should go into CmdLine.hs under runCmdLine
-- This accepts --fatspecs "oiltype:value, oiltype:value,..." options ie
-- the fatspecs option expects a comma-separated list of oiltype:value fields
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
--}
