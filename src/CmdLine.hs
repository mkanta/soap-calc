module CmdLine
  (module CmdLine
  ,module Options.Applicative
  ,module Data.Semigroup
  )
  where
-- TODO
-- This is to contain the command line parsing routines using
-- optparse-applicative
import Options.Applicative
import Data.Semigroup ((<>))

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
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
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
{-
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
