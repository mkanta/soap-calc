{-# LANGUAGE OverloadedStrings #-}

module CmdLine
  (module CmdLine
  ,module Opts           --Options.Applicative
  ,module Data.Semigroup --apparently only re-exports (<>)
  )
  where

-- This is to contain the command line parsing routines using
-- optparse-applicative

import Options.Applicative as Opts
import Data.Semigroup ((<>))
import Text.ParserCombinators.ReadP
import Data.Text as T

-- TODO: the amount should be Positive, not Rational. How to handle errors?
-- In IO because the amount comes from an option string. FatOpts should have
-- an option for water amounts, set to a Percentage of 0.33 by default
-- |The datatype for the fat specification consisting of a name for the fat
-- and the amount to be used, interpreted as a weight. This is meant to be
-- used for input, see its Read instance and parser below.
data FatSpec = FatSpec {
   name :: T.Text
  ,amount :: Rational
  } deriving Show

-- |How to get a FatSpec from a String, essentially a "name:amount" string,
-- and a [FatSpec] as FatSpec strings separated by commata, for now. See the
-- parsers below for more detail.
instance Read FatSpec where
  readsPrec _ = readP_to_S parsePFatSpec
  readList = readP_to_S parsePFatSpecList
-- The "name:number" fields separated by comma,
-- TODO semicolon, perhaps white space, should give a [FatSpec]
-- TODO do we need a readList for [FatSpec] or what is the default?
-- write Main to parse for FatOpts and then test
-- There seems to be no requirement to implement readList when defining
-- a Read instance
-- instance Read [FatSpec] where
-- readsPrec = readP_to_S (sepBy1 parsePFatSpec fatSpecSep)
-- TODO: readList needs to return a one element list because auto doesn't
-- accept ambiguous parses, only one element lists [(value,"")] this needs to
-- be changed to something more efficient, that Prelude.last thing is only
-- for testing
-- 
-- |The parser for Fat Specifications as in FatSpec. It reacts to strings of
-- the form "name:amount" where name is parsed liberally as anything not
-- containing the character ':'. It needs to allow for white space for things
-- such as "Olive Oil". Amount is parsed as a Double
-- White spaces surrounding the name are stripped before constructing the
-- FatSpec
parsePFatSpec :: ReadP FatSpec
parsePFatSpec = do
  name <- munch1 (/= ':') -- ^ This eats white space, no need for skipSpaces
  char ':'
  amount <- parsePDouble  -- ^ And so does this
  return (FatSpec (T.strip (T.pack name)) (toRational amount))
-- TODO: this should check if T.strip (T.pack name) is "", or, alternatively
-- if it is in the database. Ideally, there should be a check if there is
-- something similar in the database, like:
-- ReadP Either [String] Text
-- parsePFatName = do
--    name <- munch1 (/= ':')
--    check if name is in the database
--    if so, return Right stripped name
--    if not, return Left suggestion list
-- |A ReadP parser for Double just using its Read instance, perhaps not a good
-- idea due to inefficiencies introduced by the ReadS parser. Check this out.
parsePDouble :: ReadP Double
parsePDouble = readS_to_P (reads::String ->[(Double,String)])

-- |Separator for lists of FatSpec fat specifications.
fatSpecSep :: ReadP Char
fatSpecSep = do
  skipSpaces 
  char ','

parsePFatSpecList :: ReadP [FatSpec]
parsePFatSpecList = do
  p <- sepBy1 parsePFatSpec fatSpecSep
  eof --This is the magic to get rid of the partial parses
  return p
  
newtype FatOpts = FatOpts --TODO use data FatOpts=... more fields expected!
   { fatSpecs :: [FatSpec]
   } deriving Show
   
-- |Parsing command line for fat amounts via option --fatspecs
--  TODO: check if this works
fatopts :: Parser FatOpts
fatopts = FatOpts
     <$> Opts.option auto
         ( long "fatspecs"
          <> help "Fat types and weights"
         )
-- Below is an example
-- define a datatype to be filled with the data, for example
data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }
-- This is meant to be filled by a string option, a boolean and an integer
-- To fill it, write a parser

{-
sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello" --doesn't have to be the same as the field name
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"--doesn't have to be the same as the field name
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"--doesn't have to be the same as the field name
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

-- strOption returns a Parser String responding to option --hello
-- switch returns a Parser Bool reponding to a --quiet or -q
-- option auto returns a Parser Int responding to option --enthusiasm
-- note that enthusiasm has a default value of 1, hence --enthusiasm is
-- optional whereas --hello is required with its absence triggering an error
-- Note that option auto is the way to go with most datatypes that are an
-- instance of Read
  
-- Here is how to use the parser:
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
-}

-- This uses info to create a ParserInfo Sample from our sample parser and
-- helper which adds a --help option. The execParser is run on the ParserInfo
-- so created and returns a Sample packed in IO which is then fed into the
-- greet function.
