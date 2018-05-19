{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-} --allow them in class declarations

module Types where

import Control.Exception as E
import System.IO.Error (userError)
import Database.Persist.TH
import Data.Ratio

-- TODO this shouldn't derive Num, we only need multiplication with a Num
-- There should be a class Coefficient for which multiplication with a Num
-- is defined
-- TODO:this should be a Positive Rational or a Percentage
newtype LyeFactor = LyeFactor Rational
    deriving (Num, Show, Read, Eq)
derivePersistField "LyeFactor" --this makes LyeFactor a valid DB datatype

newtype PotFactor = PotFactor Rational
    deriving (Num, Show, Read, Eq)
derivePersistField "PotFactor" --dito for PotFactor

-- TODO:make sure NewPositive is not exported
-- The way it stands, something like NewPositive "5.1" would be possible but
-- this is acceptable since NewPositive is not exported. We're using newtype
-- instead of a GADT for generalized newtype deriving.
newtype Positive a = NewPositive a deriving (Ord,Real,Eq,Show)
--data Positive a where --here NewPositive "5" is no longer possible
--  NewPositive ::(Num a, Ord a)=> a -> Positive a
--  deriving (Ord,Real,Eq,Show) --but no generalized newtype deriving either

--Num has to be explicitly instantiated because of - and fromInteger.
--TODO: should negativity be an error or should we throw a handmade exception?
data NotPositive = NotPositive deriving (Show)
instance E.Exception NotPositive
-- TODO:the errors here should really come from error because they can't
-- be fixed by the user, the programmer should check if -, fromInteger will
-- work before using them. User error should only be in the constructor (and
-- even there the value should be checked before applying it)
instance (Num a, Ord a) => Num (Positive a) where
  (+) (NewPositive x) (NewPositive y)=NewPositive (x+y)
  (-) (NewPositive x) (NewPositive y)
    |x>y=NewPositive (x-y)
    |otherwise = E.throw $ userError "Difference of Positives not Positive"
  (*) (NewPositive x) (NewPositive y)=NewPositive (x Prelude.* y)
  abs = id
  signum _ = NewPositive 1
  fromInteger  x
    |x>0= NewPositive $ fromInteger x
    |otherwise = E.throw $ userError "No Positive from negative Integer"

-- | A type for Percentage represented as a value between 0 and 1.
-- Maybe this should just be a Double as in
-- newtype Percentage = MkPercentage Double --or
-- newtype Percentage = MkPercentage Rational deriving (Eq,Show)
-- Or maybe this could have a phantom type to indicate whether this should
-- be in % or as probability
-- But then, maybe someone comes up with a better Real than the existing one.
data Percentage = forall a.Real a => MkPercentage a
-- | A constructor for Percentage, this should be the only way to construct
-- a Percentage outside of this module because its value should be between
-- 0 and 1.
instance Show Percentage where
  show (MkPercentage x) = show (toRational x)
mkPercentage :: Real a => a -> Percentage
mkPercentage x 
  | x>0 && numerator y <= denominator y = MkPercentage x 
  | otherwise = error "Percentage out of range"
  where y=toRational x
-- | Multiplication of a Percentage with a real fractional.
-- Using just * would make it ambiguous. To use * without ambiguity it would
-- need to be defined in an explicit instance of Num but then the types
-- are not right.
-- Note that the compiler selects ambiguous symbols only by typeclass,
-- not by type, which is called Type Directed Name Resolution and would
-- lead to a combinatorial explosion in the presence of polymorphism.
-- So no overloaded functions.
infixl 7 |*|
(|*|) :: (Real a, Fractional b) => Percentage -> a -> b
(|*|) (MkPercentage x) y = fromRational $ (toRational x) Prelude.* (toRational y)

data Pos = forall a . Num a => Pos a
-- five = Pos "5" -- won't work, string not a Num
-- toInteger::Pos -> Integer
-- toInteger (Pos x) = x --won't work, types not unified can't be made Integer
-- extract (Pos x) = x --won't work, type of x on left not unified with right
abs (Pos x) = Pos (Prelude.abs x)
-- This doesn't work: types of x, y are not unified so considered different,
-- thus addition does not work
-- add (Pos x) (Pos y) = Pos (x+y)
-- or data Percentage where
--       Percentage::Real a => a->Percentage
-- so we need to check if positive and numerator < denominator
-- need a multiplication (*) :: Num a => Percentage -> a -> a
-- but how to define that? (*) (Percentage x) y = x*y --won't work probably
-- but (*) toRational x * y, so Real a => Percentage -> a -> Rational works
{-
--This is probably not useful, a should be a type, possibly a Num with redefined
--arithmetic that complains when the result is negative?
-- Positive should be derived from Real because its parameter is constrained
-- by Num and Ord
-- class Real a => Positive a where
--    mkPositive::a->(a->Bool)->? -- This would require a value and a check
-- Have a Checked class : mkChecked::a->(a->Bool)->a, this could also be
-- a constructor of a GADT
class Positive (a :: * -> *) where
  toPositive :: Num b => b -> a b
  toPositiveM :: (Num b, Monad m) => b -> m (a b)
  plus :: Num b => a b -> a b -> a b
-- TODO: a class Positive, a class Percentage, a class Units,
-- Positive and Percentage should have error types that can be thrown
-- they should have intelligent constructors that reject inits with unsuitable
-- values with throwError or throw, something like
-- TODO should be like this:
-- then Positive a should be an instance of Num, Ord, Real but Num not derived
-- newtype Positive a = NewPositive a deriving (Ord, Real, Eq, Show)
-- instance Num a => Num (Positive a) where --...now define arithmetic
-- Make a class PosReal requiring intelligent constructors:
-- class Real a => PosReal a where --put an associated type PosReal?
--     mkPositive :: a -> ?
--     mkPositiveM ::(Monad m)=> a -> m a
-- this works, too, but runs into problems with Show instances
-- data NotPositive = NotPositive String deriving Show
instance E.Exception NotPositive

-- This is pretty much useless with Num because of the way it pattern-matches:
-- it is pretty much impossible to extract the number from the container.
-- With Real, however, it can be pattern matched using toRational as below.
--Note this is equivalent to the GADT
data Pos2 where
  MkPos2::Real a => a->Pos2
-- TODO:Test this out!
-- fromPos2 (MkPos2 x)= x -- same thing as Pos, doesn't work
fromPos2 (MkPos2 x)= toRational x --this works as expected
instance Show Pos where
--    showsPrec pr (Pos p) = showsPrec pr p
    show (Pos p) = show $ toRational p

--Something like fromPos (Pos x)=x is not going to work because the result is
--not a concrete type, the type can't escape the pattern match. But this
--works since toRational returns a concrete type.
fromPos :: Pos -> Rational
fromPos (Pos x) = toRational x

toPos :: Real a => a -> Pos
toPos x|x>0 = Pos x
       |otherwise = E.throw NotPositive
{-
toPositiveM x| x>0 = return (Positive x)
             |otherwise = throwError (NotPositive x)
            
-}
-}
