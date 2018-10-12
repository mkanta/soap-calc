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
import Data.String
import Data.Text as T

-- |Run from command line using option --fatspec <fatname>:<amount> possibly
-- multiple times on command line, this should parse input and complain or
-- calculate
runCmdLine :: IO()
runCmdLine = execParser opts >>= putStrLn . show
  where
    opts = info (multiFatOpts <**> helper)
      ( fullDesc
     <> progDesc "Print a fat specification"
     <> header "fat-opts - a test for optparse-applicative" )

execParseStrings = execParserPure defaultPrefs opts 
  where
     opts = info (multiFatOpts <**> helper)
       ( fullDesc
      <> progDesc "Print a fat specification"
      <> header "fat-opts - a test for optparse-applicative" )
   
-- |Same as runCmdLine but with option string as argument ignoring
-- options from command line. This should probably be [s] -> IO()
-- because getArgs from System.Environment returns IO [String]
runCmdLineWith :: IsString s => s-> IO()
runCmdLineWith = undefined

-- |Parse for multiple occurences of --fatspec
multiFatOpts :: Parser FatOpts
multiFatOpts = FatOpts
   <$> multiRead (Opts.long "fatspec" <> Opts.help "Fat name and weight as name:double")
   
-- |The type to be filled by command-line option parsing
-- TODO use data FatOpts=... more fields expected!
newtype FatOpts = FatOpts 
   { fatSpecs :: [FatSpec]
   } deriving Show

-- |Parse for multiple occurences of datatype which is a Read
multiRead :: Read a => Mod OptionFields [a] -> Parser [a]
multiRead desc = Prelude.concat <$> some single
   where single = Opts.option (Opts.auto >>= \x -> return [x]) desc

   
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

-- |The parser for Fat Specifications as in FatSpec. It reacts to strings of
-- the form "name:amount" where name is parsed liberally as anything not
-- containing the character ':'. It needs to allow for white space for things
-- such as "Olive Oil". Amount is parsed as a Double
-- White spaces surrounding the name are stripped before constructing the
-- FatSpec
-- TODO: if name=="" there should be a pfail, should we check here whether
-- the name is in the database? Probably not because ReadP is deficient
-- when it comes to error reporting. On the other hand, when parsing a list
-- we'd like to know which one is not in the database
-- and provide alternatives.
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
-- TODO: maybe add some more list separators, using something like
-- satisfy (`elem` sepList) where sepList is a list of separators
-- instead of char ','
fatSpecSep :: ReadP Char
fatSpecSep = do
  skipSpaces 
  char ','

-- |The parser for lists of FatSpec entries. Note that sepBy1 introduces
-- nondeterminism due to its use of many. Since option auto only accepts
-- unique parses with no input left, eof is used to get rid of the partial
-- parses.
-- TODO:there may be a more efficient way to do this, such as a recursion
-- using <++ rather than +++ as used by many.
parsePFatSpecList :: ReadP [FatSpec]
parsePFatSpecList = do
  p <- sepBy1 parsePFatSpec fatSpecSep
  eof -- ^ This is the magic to get rid of the partial parses
  return p

   
-- |Parsing command line for fat amounts via option --fatspecs
fatopts :: Parser FatOpts
fatopts = FatOpts
     <$> Opts.option auto
         ( long "fatspecs"
          <> help "Fat types and weights"
         )
   
-- Below is an example
{-
-- define a datatype to be filled with the data, for example
data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }
-- This is meant to be filled by a string option, a boolean and an integer
-- To fill it, write a parser

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
-- To parse a string use
-- execParserPure defaultParserPrefs opts optionstring
-- and apply renderFailure as in handleParseResult but without
-- the IO to create a custom error message
{- Multiple use of an option:
data MyOptions = MyOptions {
    aFiles :: [String]
  , bFiles :: [String] }

multiString desc = concat <$> some single
  where single = option (str >>= return . words) desc

config :: Parser MyOptions
config = MyOptions
     <$> multiString (short 'a' <> long "aFiles" <> help "Use quotes/multiple")
     <*> multiString (short 'b' <> long "bFiles" <> help "Use quotes/multiple")
-}
