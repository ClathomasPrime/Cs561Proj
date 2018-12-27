{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Control.Lens.TH
import Data.Function


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
  { _queryManipulator :: AS
  , _queryNextHop :: AS
  , _queryDestination :: AS
  } deriving(Show, Eq, Ord, Read)
makeLenses ''Query

--------------------------------------------------------------------------------
-- "Behavioral"
--------------------------------------------------------------------------------

type ExportFilter = AS -> Path -> Bool

data ExportStrategy
  = HonestFilteredExport ExportFilter
  | ManipulatorExport (Map AS Path -> AS -> AS -> Maybe Path)
  -- ^ forward table -> requester -> dest ->
  --   ((Nothing if request denied,
  --     Just [] for claim "I have no path", Just path o.w.))
  -- ^ Note: we almost certainly want to let manipulators tell the truth
  -- based on certain conditions... maybe play this by ear.

data QueryStrategy
  = HonestAnswerQueries
  | ManipulatorAnswerQueries
  -- ^ add to this type if want to consider more advanced querying strategies

type PathPref = Path -> Maybe Double
-- ^ NOTE: Must satisfy:
--   - [] -> Just 0

data ForwardStrategy
  = HonestForwardByTable
  | ManipulatorForwardHardcoded [AS]

--------------------------------------------------------------------------------
-- One AS
--------------------------------------------------------------------------------

-- | hardcoded and static per each AS
data AsData = AsData
  -- | These first things should be relatively static:
  { _asNumber :: AS
  , _asExportStrategy :: ExportStrategy
  -- , asExportFilter :: ExportFilter
  , _asPathPref :: PathPref
  -- ^ Nothing if path isn't allowed
  , _asQueryStrategy :: QueryStrategy
  , _asForwardStrategy :: ForwardStrategy

  -- | The following should change a bunch:

  , _asForwardTable :: Map AS Path
  -- ^ unreachable is represented by an empty list
  -- Invariant: As i should get path [i] to dest i.
  , _asPreviousQueries :: [Query]
  -- ^ Queries you've already considered
  }
makeLenses ''AsData

instance Show AsData where
  show d = "AsData " ++ show (view asNumber d)
    ++ " (Forwarding: " ++ showForwards (view asForwardTable d)
    ++ if not $ null prevQs
          then ", PreviousQueries: " ++ show prevQs
          else ""
    ++ ")"
    where prevQs = view asPreviousQueries d

showForwards :: Map AS [AS] -> String
showForwards table = show . filter phi $ Map.toList table
  where phi (_, []) = False
        phi (a, [b])
          | a == b = False
        phi _ = True

--------------------------------------------------------------------------------
-- The network
--------------------------------------------------------------------------------

data NetworkData = NetworkData
  { _networkAsNumbers :: [AS]
  , _networkTopology :: Map AS [AS]
  -- ^ should be bidirectional
  , _networkMessages :: Map AS [(AS,Query)]
  -- ^ Confusingly, this is not used for normal BGP operation,
  -- just for next-hop queries
  , _networkAses :: Map AS AsData
  } deriving(Show)
makeLenses ''NetworkData

equalRoutingTables :: NetworkData -> NetworkData -> Bool
equalRoutingTables net1 net2
  = Map.foldl (&&) True
    $ Map.intersectionWith compareAses (view networkAses net1) (view networkAses net2)
  where compareAses state1 state2 =
          view asForwardTable state1 == view asForwardTable state2

emptyNetworkMessages :: NetworkData -> Bool
emptyNetworkMessages network
  = all null (view networkMessages network)
  -- ^ test this maybe sense refactor..

