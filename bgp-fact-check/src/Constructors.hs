module Constructors where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Types

-- (useful for inputting bidirectional edges)
reflexivize :: Ord a => Map a [a] -> Map a [a]
reflexivize m = Map.foldlWithKey updateAll m m
  where updateAll m' k as = foldl (updateOne k) m' as
        updateOne k m' a = Map.alter (ins k) a m'
        ins k Nothing = Just $ [k]
        ins k (Just bs) = Just $ if k `elem` bs then bs else k:bs

--------------------------------------------------------------------------------
-- Export Filters
-- type ExportFilter = AS -> Path -> Bool

exportAll :: ExportFilter
exportAll _ _ = True

exportAllOrNothing :: [AS] -> ExportFilter
exportAllOrNothing allowed as _ = as `elem` allowed


--------------------------------------------------------------------------------
-- Path Preferences
-- type PathPref = Path -> Maybe Double

policyNextHop :: [AS] -> PathPref
policyNextHop neighbors (u:a:as) =
  case elemIndex a neighbors of
       Nothing -> Nothing -- << Not allowed to route through
       Just i -> Just . fromIntegral $ length (a:as) - i
policyNextHop neighbors _ = Just 0
-- ^ length zero path must be allowed in case you can't reach dest
-- ^ length one path == you are the destination

policyDifferentialNextHop :: [(AS, [AS])] -> PathPref
policyDifferentialNextHop = undefined
-- ^ for each destination, the prefered next-hop can change.


--------------------------------------------------------------------------------
-- Attraction Preferences
-- type AttractionPref = Path -> Double

attractNothing :: AttractionPref
attractNothing _ = 0

attractVolume :: [AS] -> AttractionPref
attractVolume victims paths =
  fromIntegral . length . filter (\v -> any (v `elem`) paths) $ victims

attractNextHop :: [AS] -> AttractionPref
attractNextHop = undefined
-- ^ customer attraction is a special case of this, I think
