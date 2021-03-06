module Constructors where

import Safe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Types

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

emptyAsData :: [AS] -> AS -> AsData
emptyAsData ases agent = AsData
  { _asNumber = agent
  , _asExportStrategy = HonestFilteredExport $ exportAllOrNothing []
  , _asPathPref = noPolicy
  , _asQueryStrategy = HonestAnswerQueries
  , _asForwardStrategy = HonestForwardByTable
  , _asForwardTable = Map.fromList $
      (agent,[agent]) : [(j,[]) | j <- ases \\ [agent]]
  , _asPreviousQueries = []
  }

-- For make `networkTopology` undirected
reflexivize :: Ord a => Map a [a] -> Map a [a]
reflexivize m = Map.foldlWithKey updateAll m m
  where updateAll m' k as = foldl (updateOne k) m' as
        updateOne k m' a = Map.alter (ins k) a m'
        ins k Nothing = Just $ [k]
        ins k (Just bs) = Just $ if k `elem` bs then bs else k:bs

--------------------------------------------------------------------------------
-- Limited cases

noPolicy :: PathPref
noPolicy _ = Nothing

policySingleDest :: AS -> [Path] -> PathPref
policySingleDest dest prefList path
  | lastMay path == Just dest =
    case elemIndex path prefList of
      Nothing -> Nothing -- << Not allowed to route through
      Just i -> Just . fromIntegral $ length prefList - i
  | otherwise = Nothing

specializeForDest :: AS -> PathPref -> PathPref -> PathPref
specializeForDest specialDest specialPref otherPref path
  | lastMay path == Just specialDest = specialPref path
  | otherwise = otherPref path

policyExplicitList :: [Path] -> PathPref
policyExplicitList prefList path =
  case elemIndex path prefList of
    Nothing -> Nothing -- << Not allowed to route through
    Just i -> Just . fromIntegral $ length prefList - i

policyExplicitSingleDest :: AS -> [Path] -> PathPref
policyExplicitSingleDest dest prefList
  = specializeForDest dest (policyExplicitList prefList) noPolicy


-- =============================================================================
-- ``Behavioral'' constructors
-- =============================================================================

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
    Just i -> Just . fromIntegral $ length neighbors - i
policyNextHop neighbors _ = Just 0
-- ^ length zero path must be allowed in case you can't reach dest
-- ^ length one path == you are the destination

policyDifferentialNextHop :: [(AS, [AS])] -> PathPref
policyDifferentialNextHop = undefined
-- ^ for each destination, the prefered next-hop can change.
