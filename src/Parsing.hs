{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec parsers for parsing the string arguments of options.
-- Using Megaparsec will facilitate error handling and recovery
module Parsing where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char -- using space parser from here at the moment, 
import qualified Text.Megaparsec.Char.Lexer as L --and there is one here, 

import CmdLine (FatSpec(..))

-- | A parser to parse a string up to the first colon. The result
-- will be the string without leading and trailing white-space.
-- This will fail if there is only space before ':' because takeWhile1P
-- needs at least one token other than ':' to be successful.
-- The predicate @(/= ':')@ could be extended to include printable characters
-- only, ie with something like isPrintChar.
-- Note: something like Parsec Void String T.Text or Parsec Void T.Text String
-- causes trouble with the types
parse2Colon :: Parsec Void T.Text T.Text
parse2Colon = space  >> takeWhile1P (Just "nonempty string before ':'") (/= ':') >>= \rslt -> char ':' >> return (T.stripEnd rslt)
-- TODO: a parser that checks if a string is in the database and uses
-- some withRecovery mechanism to handle the case where it isn't: need
-- more info from the user, ie the soapification values, or a selection from
-- the database that is similar. This can be chained after parse2Colon

-- | How to consume space at the end of a lexeme, here using @space@ from
-- "Text.Megaparsec.Char", which eats none or more white-spaces.
lexeme :: Parsec Void T.Text a -> Parsec Void T.Text a
lexeme = L.lexeme space

-- | And this is how it is used:
--   Parse a positive number, accepts integer or double input optionally
--   followed by white space, the @lexeme@ part and
--   outputs it as a double.
positive :: Parsec Void T.Text Double
positive = lexeme $ try L.float <|> (fromIntegral <$> L.decimal)

-- |A Helper function:
-- parse2Eof p is the same as parser p followed by eof but returning
-- the result of p in case of success rather than m () as would be the
-- case with p >> eof
parse2Eof :: MonadParsec e s m => m b -> m b
parse2Eof p = p >>= \rslt -> eof >> return rslt

-- Experimental: FatSpecD with double rather than Rational
data FatSpecD = FatSpecD {
  ftype :: T.Text
  ,famount :: Double
  }
-- |The mega-parser for the fat specification.
fatSpecDParser :: Parsec Void T.Text FatSpecD
fatSpecDParser = FatSpecD <$> (parse2Colon) <*> (try (space >> parse2Eof positive) <?> "positive double or integer")
