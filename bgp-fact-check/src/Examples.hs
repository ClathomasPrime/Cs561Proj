{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Examples where

import Data.Maybe
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Constructors
import Dynamics
import FactCheck

genericDataState :: [AS] -> AS -> PathPref -> (AsData, AsState)
genericDataState asNums i prefs =
  ( AsData
    { asNumber = i
    , asExportStrategy = HonestFilteredExport exportAll
    , asPathPref = prefs
    , asAttractionPref = attractNothing
    , asQueryStrategy = HonestAnswerQueries
    }
  , emptyAsState asNums i
  )

badGadget :: NetworkData
badGadget = NetworkData
  { networkAsNumbers = asNums
  , networkTopology = reflexivize $ Map.fromList
      [ (0, [1,2,3])
      , (1, [2,3])
      , (2, [4])
      , (3, [4])
      ]
  , networkMessages = Map.fromList . fmap (,[]) $ asNums
  , networkAses = Map.fromList
      [(i, dataState i) | i <- asNums]
  }
  where asNums = [0..4]
        genericDataState' = genericDataState asNums
        dataState 0 = genericDataState' 0
          (policyExplicitSingleDest 0 [])
        dataState 1 = genericDataState' 1
          (policyExplicitSingleDest 0 [[1,3,0], [1,0]])
        dataState 2 = genericDataState' 2
          (policyExplicitSingleDest 0 [[2,1,0], [2,0]])
        dataState 3 = genericDataState' 3
          (policyExplicitSingleDest 0 [[3,4,2,0],[3,0]])
        dataState 4 = genericDataState' 4
          (policyExplicitSingleDest 0 [[4,2,0],[4,3,0]])


inconsistentPolicyHonest :: NetworkData
inconsistentPolicyHonest = NetworkData
  { networkAsNumbers = asNums
  , networkTopology = reflexivize $ Map.fromList
      [ (1, [2,3,4]) , (2, [3]) , (3, [4]) ]
  , networkMessages = Map.fromList . fmap (,[]) $ asNums
  , networkAses = Map.fromList
      [(i, dataState i) | i <- asNums]
  }
  where asNums = [1,2,3,4]
        genericDataState' = genericDataState asNums
        dataState 1 = genericDataState' 1
          (policyExplicitSingleDest 3 [[1,2,3], [1,3]])
        dataState 2 = genericDataState' 2
          (policyExplicitSingleDest 3 [[2,3]])
        dataState 3 = genericDataState' 3
          (policyExplicitSingleDest 3 [])
        dataState 4 = genericDataState' 4
          (policyExplicitSingleDest 3 [[4,1,3],[4,3],[4,1,2,3]])

inconsistentPolicyManip :: NetworkData
inconsistentPolicyManip = NetworkData
  { networkAsNumbers = asNums
  , networkTopology = reflexivize $ Map.fromList
      [ (1, [2,3,4]) , (2, [3]) , (3, [4]) ]
  , networkMessages = Map.fromList . fmap (,[]) $ asNums
  , networkAses = Map.fromList
      [(i, dataState i) | i <- asNums]
  }
  where asNums = [1,2,3,4]
        genericDataState' = genericDataState asNums

        dataState i@1 =
          ( AsData
            { asNumber = i
            , asExportStrategy = ManipulatorExport . curry $
                \case (4,3) -> Just [1,3]
                      _ -> Nothing
            , asPathPref = policyExplicitSingleDest 3 [[1,2,3], [1,3]]
            , asAttractionPref = attractNothing
            , asQueryStrategy = ManipulatorAnswerQueries ()
            }
          , emptyAsState asNums i
          )

        dataState 2 = genericDataState' 2
          (policyExplicitSingleDest 3 [[2,3]])
        dataState 3 = genericDataState' 3
          (policyExplicitSingleDest 3 [])
        dataState 4 = genericDataState' 4
          (policyExplicitSingleDest 3 [[4,1,3],[4,3],[4,1,2,3]])

dm = fromJust $ initializeMessages <$> bgpConverge inconsistentPolicyManip
