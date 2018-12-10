{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Map (Map)
import qualified Data.Map as Map

type AS = Int

type ExportFilter = AS -> Path -> Bool

type PathPref = Path -> Maybe Double
-- ^ NOTE: Must satisfy:
--   - [] -> Just 0

type AttractionPref = [Path] -> Double
  -- ^ todo: maybe this type isn't right
  -- ^ todo: maybe this should go into BgpStrategizer.

-- | hardcoded and static per each AS
data AsData = AsData
  { asNumber :: AS
  , asExportFilter :: ExportFilter
  , asPathPref :: PathPref
  -- ^ Nothing if path isn't allowed
  , asAttractionPref :: AttractionPref
  }

instance Show AsData where
  show d = show (asNumber d)

-- changes over life of protocol
data AsState = AsState
  { asForwardTable :: Map AS [AS]
  -- ^ unreachable is represented by an empty list
  , asNextHopQueries :: ()
  -- , asExtraInfo
  --   :: forall a. BgpStrategizer a => BgpStrategyState a
    -- ^ unclear if this is needed
  } deriving(Show)

type Path = [AS]

data QueryMessage
  = QueryForward Query
  | AnswerForward Query Bool
  deriving(Show, Eq, Ord, Read)

data Query = Query
  { queryAsker :: AS
  , queryManipulator :: AS
  , queryNextHop :: AS
  , queryDestination :: AS
  } deriving(Show, Eq, Ord, Read)

data NetworkData = NetworkData
  { networkAsNumbers :: [AS]
  , networkTopology :: Map AS [AS]
  -- ^ should be bidirectional
  , networkMessages :: Map AS [(AS,QueryMessage)]
  -- ^ Confusingly, this is not used for normal BGP operation,
  -- just for next-hop queries
  , networkAses :: Map AS (AsData, AsState)
  } deriving(Show)

-- data NetworkState = NetworkState
--   {
--   } deriving(Show)

equalRoutingTables :: NetworkData -> NetworkData -> Bool
equalRoutingTables net1 net2
  = Map.foldl (&&) True
    $ Map.intersectionWith compareAses (networkAses net1) (networkAses net2)
  where compareAses (_, state1) (_, state2) =
          asForwardTable state1 == asForwardTable state2

--------------------------------------------------------------------------------

type BgpLocalState = ()

type BgpAction = ()

class BgpStrategizer m where
  data BgpStrategyState m :: *

  actStrategically :: BgpStrategyState m -> BgpLocalState -> BgpAction

