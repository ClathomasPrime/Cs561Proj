{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dynamics where

import Data.Maybe
import Control.Monad
import Control.Monad.Identity
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Safe.Foldable
import Control.Lens

import Types
import Monad

import Debug.Trace

bgpConvergeAllAct :: NetworkData -> Maybe NetworkData
bgpConvergeAllAct network = bgpConverge' 20 network (bgpStepAllAct network)
  where bgpConverge' _ prev current
          | prev `equalRoutingTables` current = Just current
        bgpConverge' 0 _ _
          = Nothing
        bgpConverge' i _ current
          = bgpConverge' (i-1) current (bgpStepAllAct current)

        bgpStepAllAct :: NetworkData -> NetworkData
        bgpStepAllAct net = execState bgpStep net
        -- ^ casts bgpStep to use StateT NetworkData Identity
        -- (i.e Identity Activator Monad)

bgpStep :: (Activator m, MonadState NetworkData m) => m ()
bgpStep = do
  activatedAgents <- activate =<< use networkAsNumbers
  mapM_ bgpActivation activatedAgents

-- | An activated AS will:
--    1) update it routing table by looking at all its neighbors
--    2) read and respond to query messages
bgpActivation :: MonadState NetworkData m => AS -> m ()
bgpActivation agent =
  let updateDest d = bgpUpdateRouteToDest agent d
   in mapM_ updateDest =<< use networkAsNumbers

bgpUpdateRouteToDest :: MonadState NetworkData m => AS -> AS -> m ()
bgpUpdateRouteToDest agent dest
  | agent == dest = return ()
bgpUpdateRouteToDest agent dest = do
  -- traceShowM (agent,dest)
  Just neighbors <- use (networkTopology . at agent)
  otherAses <- use networkAses
  let neighborData = Map.filterWithKey (\k _ -> k `elem` neighbors) otherAses
      exports = Map.map (\u -> exportTo u agent dest) neighborData
      -- ^ get the path in neighbors routing tables (provided they want to export them)

      availablePaths :: [Path]
      availablePaths = Map.foldlWithKey accum [] exports
      accum ps _ Nothing = ps
      accum ps _ (Just []) = ps
      accum ps neighbor (Just p)
        | agent `elem` p = ps
        | otherwise = (agent:p) : ps
      -- ^ add yourself to the paths

  Just agentData <- use (networkAses . at agent)
  let rankedPaths = [(rank, p)
        | p <- availablePaths
        , rank <- maybeToList $ view asPathPref agentData p]
      favoritePath = snd $ maximum $ (-1, []) : rankedPaths
      updateForward s = updateForwardTable s dest favoritePath

  (networkAses . ix agent) %= updateForward -- network

--------------------------------------------------------------------------------

updateForwardTable :: AsData -> AS -> Path -> AsData
updateForwardTable asData dest path
  = set (asForwardTable . at dest) (Just path) asData

-- | emit `Nothing' if request for a path denied
-- emit `Just []' if no path yet in forward table.
exportTo :: AsData -> AS -> AS -> Maybe Path
exportTo agentData requester dest =
  case view asExportStrategy agentData of
    HonestFilteredExport exportFilter ->
      let Just path = Map.lookup dest (view asForwardTable agentData)
       in if exportFilter requester path
             then Just path
             else Nothing
    ManipulatorExport strategy ->
      strategy (view asForwardTable agentData) requester dest

-- | ignoring lies and stuff
realPathToDest :: NetworkData -> AS -> AS -> Maybe Path
realPathToDest _ source dest
  | source == dest = Just [dest]
realPathToDest network source dest =
  let Just sourceState = view (networkAses . at source) network
   in case Map.lookup dest (view asForwardTable sourceState) of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (nextHop:_) -> fmap (source :) (realPathToDest network nextHop dest)
