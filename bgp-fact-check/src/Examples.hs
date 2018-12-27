{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Examples where

import Data.Maybe
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens

import Types
import Constructors
import Dynamics
import FactCheck

genericDataState :: [AS] -> AS -> PathPref -> AsData
genericDataState asNums i prefs =
  emptyAsData asNums i
    & asPathPref .~ prefs
    & asExportStrategy .~ HonestFilteredExport exportAll

genericNetworkData :: [AS] -> [(AS, [AS])] -> (AS -> AsData) -> NetworkData
genericNetworkData asNums topo dataState = NetworkData
  { _networkAsNumbers = asNums
  , _networkTopology = reflexivize $ Map.fromList topo
  , _networkMessages = Map.fromList . fmap (,[]) $ asNums
  , _networkAses = Map.fromList
      [(i, dataState i) | i <- asNums]
  }

--------------------------------------------------------------------------------

badGadget :: NetworkData
badGadget = genericNetworkData
  [0..4]
  -- topology:
  [ (0, [1,2,3]), (1, [2,3]), (2, [4]), (3, [4]) ]
  dataState
  where thing i paths = genericDataState [0..4] i
          (policyExplicitSingleDest 0 paths)
        dataState 0 = thing 0 []
        dataState 1 = thing 1 [[1,3,0], [1,0]]
        dataState 2 = thing 2 [[2,1,0], [2,0]]
        dataState 3 = thing 3 [[3,4,2,0],[3,0]]
        dataState 4 = thing 4 [[4,2,0],[4,3,0]]

inconsistentPolicyHonest :: NetworkData
inconsistentPolicyHonest = genericNetworkData
  [1..4]
  [ (1, [2,3,4]), (2, [3]), (3, [4]) ]
  dataState
  where thing i paths = genericDataState [1..4] i
          (policyExplicitSingleDest 3 paths)
        dataState 1 = thing 1 [[1,2,3], [1,3]]
        dataState 2 = thing 2 [[2,3]]
        dataState 3 = thing 3 []
        dataState 4 = thing 4 [[4,1,3],[4,3],[4,1,2,3]]

inconsistentPolicyManip :: NetworkData
inconsistentPolicyManip =
  inconsistentPolicyHonest
  & networkAses . ix 1 . asExportStrategy .~ ManipulatorExport strat
  & networkAses . ix 1 . asQueryStrategy .~ ManipulatorAnswerQueries
  where
    -- strat :: (requester) -> (dest) -> Maybe Path
    strat _ 4 3 = Just [1,3]
    strat _ _ _ = Nothing

--------------------------------------------------------------------------------

grandmaHonest :: NetworkData
grandmaHonest = genericNetworkData
  [1..6]
  [ (1, [2,6]), (2, [3,4,5]), (3, [5]), (4, [5]), (5, [6]) ]
  dataState
  where thing i neighborRank = genericDataState [1..6] i
          (specializeForDest 6 (policyNextHop neighborRank) noPolicy)
        dataState 1 = genericDataState [1..6] 1
          (policyExplicitSingleDest 6 [[1, 6]])
        dataState 2 = thing 2 [5,4,1]
          & asExportStrategy .~ HonestFilteredExport exp2
        dataState 3 = thing 3 [2,5]
        dataState 4 = thing 4 [5,2]
        dataState 5 = thing 5 [6,3,2]
        dataState 6 = thing 6 []

        exp2 3 (_:1:_) = False -- ^ no export to 3
        exp2 _ _ = True

grandmaManip :: NetworkData
grandmaManip = grandmaHonest
  & networkAses . ix 5 . asExportStrategy .~ ManipulatorExport strat
  & networkAses . ix 5 . asQueryStrategy .~ ManipulatorAnswerQueries
  where -- strat :: (fwrd table) -> (requester) -> (dest) -> Maybe Path
        strat tab fool 6 = Just [5,2,1,6]
        strat _ _ _ = Nothing

--------------------------------------------------------------------------------

dm = fromJust $ bgpConvergeAllAct grandmaManip
fc = factCheckConvergeAllAct dm
