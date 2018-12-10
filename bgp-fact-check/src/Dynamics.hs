{-# LANGUAGE RecordWildCards #-}
module Dynamics where

import Data.Maybe
import Control.Monad
import Control.Monad.Identity
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Safe.Foldable

import Types

import Debug.Trace

-- (This is a possible idea if we make a monad stack)
-- class Monad m => Activator m where
--   activated :: [a] -> m [a]
--
-- instance Activator Identity where
--   activated = Identity
--
-- instance Activator IO where
--   activated = filterM randIO
--     where randIO _ = randomRIO (True, False)

bgpStepAllAct :: NetworkData -> NetworkData
bgpStepAllAct network@(NetworkData{..}) =
  foldl bgpActivation network networkAsNumbers

-- | An activated AS will:
--    1) update it routing table by looking at all its neighbors
--    2) read and respond to query messages
bgpActivation :: NetworkData -> AS -> NetworkData
bgpActivation network@(NetworkData{..}) agent =
  let Just messages = Map.lookup agent networkMessages
      -- ^ do nothing with this so far

      updateDest ntw d = bgpUpdateRouteToDest ntw agent d
      updatedForwardTables = foldl updateDest network networkAsNumbers

   in updatedForwardTables

bgpUpdateRouteToDest :: NetworkData -> AS -> AS -> NetworkData
bgpUpdateRouteToDest network@(NetworkData{..}) agent dest =
  let Just neighbors = Map.lookup agent networkTopology
      neighborData = Map.filterWithKey (\k _ -> k `elem` neighbors) networkAses
      exports = Map.map (\(u,v) -> exportTo u v agent dest) neighborData
      -- ^ get the path in neighbors routing tables (provided they want to export them)

      availablePaths :: [Path]
      availablePaths = Map.foldlWithKey accum [] exports
      accum ps _ Nothing = ps
      accum ps _ (Just []) = ps
      accum ps neighbor (Just p)
        | agent `elem` p = ps
        | otherwise = (agent:p) : ps
      -- ^ add yourself to the paths

      Just (agentData@(AsData{..}), agentState@(AsState{..}))
        = Map.lookup agent networkAses
      rankedPaths = [(rank, p)
        | p <- availablePaths, rank <- maybeToList $ asPathPref p]

      favoritePathMay = maximumMay rankedPaths
      updateForward path (d,s) = (d,updateForwardTable s dest path)
   in case favoritePathMay of
        Nothing -> network
        Just (_,path) -> network {
          networkAses = Map.adjust (updateForward path) agent networkAses }

updateForwardTable :: AsState -> AS -> Path -> AsState
updateForwardTable asState dest path
  = asState { asForwardTable =
    Map.alter addPath dest (asForwardTable asState) }
  where addPath _ = Just path

-- | emit `Nothing' if request for a path denied
-- emit `Just []' if no path yet in forward table.
exportTo :: AsData -> AsState -> AS -> AS -> Maybe Path
exportTo agentData@(AsData{..}) agentState@(AsState{..}) requester dest =
  case Map.lookup dest asForwardTable of
    Just path -> if asExportFilter requester path
      then Just path
      else Nothing
    Nothing -> Nothing


-- | ignoring lies and stuff
realPathToDest :: NetworkData -> AS -> AS -> Maybe Path
realPathToDest _ source dest
  | source == dest = Just [dest]
realPathToDest network@(NetworkData{..}) source dest =
  let Just (_, sourceState) = Map.lookup source networkAses
   in case Map.lookup dest (asForwardTable sourceState) of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (nextHop:_) -> fmap (source :) (realPathToDest network nextHop dest)
