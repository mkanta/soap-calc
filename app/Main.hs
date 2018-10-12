module Main where

import Lib
import CmdLine
import Gui

import System.Environment (getArgs)

-- TODO: this should runGui only if there is a -i, --interactive flag set?
--       and then initialise runGui with fat options if any?
--       or when there are no options on the command line at all.
main :: IO ()
-- main = runCmdLine
main = runGui
