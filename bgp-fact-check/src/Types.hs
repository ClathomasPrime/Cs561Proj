{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type AS = Int

type Path = [AS]
-- ^ Invariant: always include source and dest in path,
--   even in forward table

--------------------------------------------------------------------------------

-- A type allowing the queries to return to the asker could look like this:
-- data QueryMessage
--   = QueryForward { getQuery :: Query, returnPath :: Path }
--   | AnswerForward { getQuery :: Query, returnPath :: Path, answer :: Bool }

data Query = Query
  { queryManipulator :: AS
  , queryNextHop :: AS
  , queryDestination :: AS
  } deriving(Show, Eq, Ord, Read)

--------------------------------------------------------------------------------
-- "Behavioral"
--------------------------------------------------------------------------------

type ExportFilter = AS -> Path -> Bool

data ExportStrategy
  = HonestFilteredExport ExportFilter
  | ManipulatorExport (AS -> AS -> Maybe Path)
  -- ^ requester -> dest ->
  --   ((Nothing if request denied,
  --     Just [] for claim "I have no path", Just path o.w.))
  -- ^ Note: we almost certainly want to let manipulators tell the true
  -- based on certain conditions... maybe play this by ear.

data QueryStrategy
  = HonestAnswerQueries
  | ManipulatorAnswerQueries
  -- ^ add to this type if want to consider more advanced querying strategies

type PathPref = Path -> Maybe Double
-- ^ NOTE: Must satisfy:
--   - [] -> Just 0

-- type AttractionPref = [Path] -> Double
  -- ^ todo: maybe this type isn't right
  -- ^ todo: maybe this should go into BgpStrategizer.

--------------------------------------------------------------------------------
-- One AS
--------------------------------------------------------------------------------

-- | hardcoded and static per each AS
data AsData = AsData
  -- | These first things should be relatively static:
  { asNumber :: AS
  , asExportStrategy :: ExportStrategy
  -- , asExportFilter :: ExportFilter
  , asPathPref :: PathPref
  -- ^ Nothing if path isn't allowed
--, asAttractionPref :: AttractionPref
  -- ^ I'm starting to think this won't be very useful.
  --   The decisions made to attract traffic aren't determined
  --   by a fixed algorithm such as BGP.
  --   Might be better to have a ``decision making'' type
  , asQueryStrategy :: QueryStrategy

  -- | The following should change a bunch:

  , asForwardTable :: Map AS [AS]
  -- ^ unreachable is represented by an empty list
  -- Invariant: As i should get path [i] to dest i.
  , asPreviousQueries :: [Query]
  -- ^ Queries you've already considered
  }

instance Show AsData where
  show d = show (asNumber d)
  -- ^ TODO: not this.

--------------------------------------------------------------------------------
-- The network
--------------------------------------------------------------------------------

data NetworkData = NetworkData
  { networkAsNumbers :: [AS]
  , networkTopology :: Map AS [AS]
  -- ^ should be bidirectional
  , networkMessages :: Map AS [(AS,Query)]
  -- ^ Confusingly, this is not used for normal BGP operation,
  -- just for next-hop queries
  , networkAses :: Map AS AsData
  } deriving(Show)

equalRoutingTables :: NetworkData -> NetworkData -> Bool
equalRoutingTables net1 net2
  = Map.foldl (&&) True
    $ Map.intersectionWith compareAses (networkAses net1) (networkAses net2)
  where compareAses state1 state2 =
          asForwardTable state1 == asForwardTable state2

emptyNetworkMessages :: NetworkData -> Bool
emptyNetworkMessages network
  = Map.foldl (\b ms -> b && null ms) True (networkMessages network)

