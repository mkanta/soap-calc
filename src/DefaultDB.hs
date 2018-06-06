{-# LANGUAGE OverloadedStrings #-}
-- TODO this should be really called DBDefaults and contain default paths
-- for the database in addition to a default database
module DefaultDB
  ( defaultList
  ) where

import Data.Text

-- TODO: maybe this should be a [(Text,Text,Text)] for equal footing with
-- databases where the inputs are essentially strings
defaultList :: Fractional a => [(Text,a,a)]
defaultList=[
  ("Almond Oil",0.1367,0.1925)
  ,("Canola Oil",0.1328,0.1870)
  ,("Coconut Oil, Refined 76",0.1910,0.2690)
  ,("Coconut Oil, Hydrogenated 92",0.1910,0.2690)
  ,("Coconut Oil, Saturated",0.2321,0.3269)
  ,("Corn Oil",0.1368,0.1972)
  ,("Flaxseed Oil",0.1358,0.1913)
  ,("Olive Oil",0.1353,0.1906)
  ,("Palm Kernel Oil",0.1777,0.2503)
  ,("Palm Oil",0.1420,0.2)
  ,("Safflower Oil, High Linoleic Acid",0.1374,0.1935)
  ,("Safflower Oil, High Oleic Acid",0.1369,0.1928)
  ,("Soybean Oil",0.1359,0.1940)
  ,("Sunflower Seed Oil",0.1358,0.1913)
  ]
