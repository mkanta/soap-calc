{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-} --allow them in class declarations
-- TODO make Percentage a fractional with recip=id and the proper fromRational
-- maybe mkPositive should be called toPositive, dito for mkPercentage
module Types (Positive
             ,mkPositive
             ,fromPositive
             ,Percentage
             ,mkPercentage
             ,fromPercentage
             ,LyeFactor(..)
             ,PotFactor(..)
             ) where

import Control.Exception as E
import System.IO.Error (userError) 
import Database.Persist.TH
import Data.Ratio

-- TODO this shouldn't derive Num, we only need multiplication with a Num
-- There should be a class Coefficient for which multiplication with a Num
-- is defined
-- TODO:this should be a Percentage, note that to use derivePersistField,
-- the datatype needs to implement Show and Read only, Num and Eq are not
-- necessary
-- | A type for Percentage represented as a value between 0 and 1.
-- Maybe this should just be a Double as in
-- newtype Percentage = MkPercentage Double --or
-- newtype Percentage = MkPercentage Rational deriving (Eq,Show)
-- Or maybe this could have a phantom type to indicate whether this should
-- be in % or as probability
-- But then, maybe someone comes up with a better Real than the existing one.
-- Note, this has to be before LyeFactor because of derivePersistField, which
-- uses template haskell and needs to know about Percentage beforehand
data Percentage = forall a.Real a => MkPercentage a

-- |showing a Percentage as a Double 
instance Show Percentage where
  show (MkPercentage x) = show (fromToRational x)

-- |Dito for reading
-- need to check if dval is in range, otherwise return a failed parse
instance Read Percentage where
  readsPrec _ s = case reads s ::[(Double,String)] of
                       [(dval,rem)]|dval>=0.0 && dval<= 1.0 -> [(MkPercentage dval,rem)]
                                   |otherwise -> []
                       _ -> []
-- |A helper function for converting Percentage to Fractional
-- Note this needs to be here because of the splice below arising from
-- derivePersistField
fromToRational :: (Real a, Fractional c) => a -> c
fromToRational = fromRational . toRational

newtype LyeFactor = LyeFactor Percentage  deriving (Read, Show)
derivePersistField "LyeFactor" -- ^this makes LyeFactor a valid DB datatype

-- A Show instance for LyeFactor which just shows the embedded Double
-- instance Show LyeFactor where
   -- show (LyeFactor (MkPercentage x)) = show ((fromToRational x) :: Double)

-- A Read instance for LyeFactor which packs a Double into a LyeFactor
-- TODO: this is wrong: dval needs to be checked for range
-- instance Read LyeFactor where
  -- readsPrec _ s = case reads s ::[(Double,String)] of
                       -- [(dval,rem)] -> [(LyeFactor (MkPercentage dval),rem)]
                       -- _ -> []
newtype PotFactor = PotFactor Percentage deriving (Read, Show)
derivePersistField "PotFactor" -- ^make PotFactor a valid DB datatype

-- A Show instance for PotFactor which just shows the embedded Double
-- instance Show PotFactor where
   -- show (PotFactor (MkPercentage x)) = show ((fromToRational x)::Double)

-- A Read instance for PotFactor which packs a Double into a LyeFactor
-- TODO: this is wrong: dval needs to be checked for range
-- instance Read PotFactor where
  -- readsPrec _ s = case reads s ::[(Double,String)] of
                       -- [(dval,rem)] -> [(PotFactor (MkPercentage dval),rem)]
                       -- _ -> []
-- TODO:make sure NewPositive is not exported
-- The way it stands, something like NewPositive "5.1" would be possible but
-- this is acceptable since NewPositive is not exported. We're using newtype
-- instead of a GADT for generalized newtype deriving.
newtype Positive a = NewPositive a deriving (Ord,Real,Eq,Show)
--data Positive a where --here NewPositive "5" is no longer possible
--  NewPositive ::(Num a, Ord a)=> a -> Positive a
--  deriving (Ord,Real,Eq,Show) --but no generalized newtype deriving either

-- |Constructor for Positive. Throwing a userError here seems justified because
-- this probably happens in input somewhere. Note, we can't use ioError, types
-- wouldnt match in branches.
mkPositive :: Real a => a -> Positive a
mkPositive x|x > 0 = NewPositive x
            | otherwise = E.throw $ userError "No Positives from negative Ord"
-- TODO:Perhaps add a
-- mkPositiveIO :: Real a => a -> IO (Positive a) where we use ioError
fromPositive :: Positive a -> a
fromPositive (NewPositive x) = x

-- |Num has to be explicitly instantiated because of subtraction and
-- fromInteger.
instance (Num a, Ord a) => Num (Positive a) where
  (+) (NewPositive x) (NewPositive y)=NewPositive (x+y)
  (-) (NewPositive x) (NewPositive y)
    |x>y=NewPositive (x-y)
    |otherwise = E.throw $ userError "(-):Difference of Positives not Positive"
    -- ^ TODO: this should probably be an outright error, there is nothing
    -- the user can do here to fix that. Dito for fromInteger.
  (*) (NewPositive x) (NewPositive y)=NewPositive (x Prelude.* y)
  abs = id
  signum _ = NewPositive 1
  fromInteger  x
    |x>0= NewPositive $ fromInteger x
    |otherwise = E.throw $ userError "fromInteger: Positive from negative Integer"

-- | A constructor for Percentage, this should be the only way to construct
-- a Percentage outside of this module because its value should be between
-- 0 and 1.
mkPercentage :: Real a => a -> Percentage
mkPercentage x 
  | x>0 && numerator y <= denominator y = MkPercentage x 
  | otherwise = error "Percentage out of range"
  where y=toRational x

-- | Multiplication of a Percentage with a Real to get a Fractional.
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

-- TODO: do we need a fromPercentage? Then instead of |*| a Rational could be
-- extracted and regular * could be used
fromPercentage (MkPercentage x) = toRational x
