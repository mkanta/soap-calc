This is a soap calculator. It calculates the amounts of oils, water and lye
required to make soap. It uses an sqlite database of oils with saponification
coefficients and a gtk interface.

At the same time testing literate haskell.

These language extensions and imports may not all be needed,
this is just a copy-paste from an sql interface that works.
They seem to be needed because of the code generated by template haskell.
For explanations see
https://www.yesodweb.com/book/persistent
\begin{code}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module SoapLib
  ( soapValues
  , initDefaultDB     -- default database if nothing else can be found
  , tuple2SoapVal     -- this is for debugging only
  , lyeUpdate     -- this is for debugging only
  , lyeWeight     -- this is for debugging only
  , packedSoapVal     -- this is for playing around only
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.List (foldl')
import Data.Text as T   --shadows Prelude and Data.List
-- import Data.Time

import Types
import DefaultDB
-- import Database.Esqueleto
-- import Database.Persist hiding ((==.)) -- probably hiding because of esqueleto
import Database.Persist 
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.Types (entityVal)
import Database.Persist.TH
\end{code}
The database schema, a SoapValue table with the type of fat, the saponification
value for lye (NaOH) and potash (KOH) (perhaps this should be a Maybe) and a
Unique constraint on the fatType.
This could have been done with a simple
\begin{code}% just for example, not really code
mkPersist sqlSettings [persistLowerCase|
SoapValue
   fatType String
   lyeFactor Rational
   potashFactor Rational
   FatID fatType
   deriving Show
|]
\end{code}%
which generates the necessary code for datatypes matching the records in the
database, ie a SoapValue record type with the given fields among others.
There are two problems with this approach:
  - At the beginning of each query, migration should occur. This is a check to
    see if the database is still what Haskell thinks it is.
  - This becomes cumbersome if more than one table is involved and potentially
    slow because.
This is where share comes in which passes information from the different
table definitions to the template functions to concatenate results.

Types LyeFactor and PotFactor were defined in module Types so there will
be no confusion between the two because they are both weight ratios describing
the weight of lye and potashh, respectively, required to saponify a given
weight of the fatType. So they are both Rationals expressing a percentage
but for different purposes. Note that they had to be defined in a different
module due to staging requirements for GHC template code, which often can
not be used in the module where it is defined.

\begin{code}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SoapValue
   fatType Text
   lyeFactor LyeFactor
   potashFactor PotFactor
   FatID fatType
   deriving Show
|]
\end{code}
Now finally some db interaction code:
\begin{code}
-- this is actually only sample code, use soapValues below with a one-element
-- list of fat names instead.
packedSoapVal dbfile fatName = runSqlite dbfile $ do
  runMigration migrateAll
  soapEntityMaybe <- getBy $ FatID fatName -- nonexhaustive pattern?
  case soapEntityMaybe of
    Just soapEntity -> return (Just (entityVal soapEntity))
    _ -> return Nothing
  -- case soapVal of
    --(SoapValue _ lyef _) -> return (Just lyef)
--return soapVal
\end{code}
Entity is a record consisting of a key and a record, in this case
of type SoapValue. Extract the lyeFactor by pattern matching SoapValue.
The SoapValue can also be extracted from the Entity by pattern-matching or with
Database.Persist.Types.entityVal

The getBy query returns a Maybe (Entity record) packed in an sql monad stack,
something like
ReaderT backend (NoLoggingT (Resource T m)) (Maybe (Entity SoapValue))
where m is a MonadUnliftIO (which could be an Id monad). The runSqlite function
unpacks this monad stack so that packedSoapVal returns an m (Maybe LyeFactor)

This could be used in a function taking a Maybe LyeFactor and returning
something in a MonadUnliftIO using >>=. Instances of MonadUnliftIO: IdentityT m,
ReaderT r m, IO

Note that runMigration has to be performed before executing queries because it
returns the necessary ReaderT SqlBackend m monad environment. It can be replaced
by runMigrationSilent which puts a list of SQL commands executed into the
ReaderT SqlBackend m monad, ie it returns ReaderT SqlBackend m [Text] instead
of ReaderT SqlBackend m ().

Here is a query for a list of fats done by a selectList query
with a filter checking for names in the fatNames list. This is actually
better than a getBy, if nothing matches there will just be an empty list
instead of a Maybe.
\begin{code}
soapValues dbfile fatNames = runSqlite dbfile $ do
  runMigration migrateAll
  --fmap (Prelude.map entityVal) $ selectList [SoapValueFatType <-. fatNames] []
  selectList [SoapValueFatType <-. fatNames] [] >>= return . Prelude.map entityVal
\end{code}
This returns an m [SoapValue].
TODO: The result should be checked for completeness, ie all fatNames found
returning a list of missing fats. First step: comparing lengths of fatNames
vs list returned. If that fails, call a difference function that returns a
list of fatNames that could not be found.

Generate a default database of saponification values from a list of tuples
(fatname, lyefactor, potfactor) in DefaultDB

\begin{code}
tuple2SoapVal (fname, lyeFac, potFac) = SoapValue fname (LyeFactor (toRational lyeFac)) ((PotFactor  .toRational) potFac) -- both ways work
initDefaultDB dbfile = runSqlite dbfile $ do
  runMigration migrateAll
  insertMany_ (Prelude.map tuple2SoapVal DefaultDB.defaultList) 
\end{code}
Now to actually calculating the lye amount:
\begin{code}
-- update function for fold, replace by lambda since it is so short
-- lyeUpdate :: Double -> (Double,Double) -> Double
lyeUpdate la (fa,lyfac) = la+lyfac*fa 
-- TODO: fatWeights should be a list of masses, ie positive doubles,
-- which brings in the issue of units
-- lyeWeight :: Num a => [a] -> [a] -> a
lyeWeight fatWeights lyeFacs = Data.List.foldl' (\la (fa,lyfac) -> la+lyfac*fa) 0 ( Prelude.zip fatWeights lyeFacs)
\end{code}
TODO:
There should perhaps be a list of fatnames found in the database, query
that one first. This should use a Conduit, though, with connect-and-resume so
that not the whole database needs to be read in at once. Use selectSource for
that. Scrolling the list should trigger a resume operation on the conduit.
