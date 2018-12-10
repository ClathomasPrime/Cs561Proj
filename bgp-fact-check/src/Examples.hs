{-# LANGUAGE TupleSections #-}
module Examples where

import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Constructors
import Dynamics

inconsistentPolicy :: NetworkData
inconsistentPolicy = NetworkData
  { networkAsNumbers = [1,2,3,4]
  , networkTopology = reflexivize $ Map.fromList
      [ (1, [2,3,4])
      , (2, [3])
      , (3, [4])
      ]
  , networkMessages = Map.fromList . fmap (,[]) $ asNums
  , networkAses = Map.fromList
      [(i, dataState i) | i <- asNums]
  }
  where asNums = [1,2,3,4]
        dataState i =
          ( AsData
            { asNumber = i
            , asExportFilter = exportAll
            , asPathPref = policyNextHop $ asNums \\ [i]
            , asAttractionPref = attractNothing
            }
          , AsState
            { asForwardTable = Map.fromList $
                (i,[i]) : [(j,[]) | j <- asNums \\ [i]]
            , asNextHopQueries = ()
            }
          )


