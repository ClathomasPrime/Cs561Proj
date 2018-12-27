{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FactCheck where

import Safe hiding (at)
import Data.Maybe
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Safe.Foldable
import Control.Lens

import Types
import Dynamics
import Monad

import Debug.Trace

-- | Right now, I'll assume that a manipulator node forwards
-- traffic in exactly one direction, and thus if one node can
-- coroborate the next-hop announced by the potential manipulator,
-- then that hop is ``cleared'' and the other nodes won't need to keep checking.
-- I'll try to point out when this manifests in code.


-- | After bgp stabilizes, run the fact checking round
factCheckConvergeAllAct :: NetworkData -> Maybe [AS]
factCheckConvergeAllAct network
  = factCheckConverge' 20 (initializeMessages network)
  where factCheckConverge' :: Int -> NetworkData -> Maybe [AS]
        factCheckConverge' 0 _ = Nothing
        factCheckConverge' i net
          | emptyNetworkMessages net = Just []
          | otherwise =
            let (caught, net') = factCheckStepAllAct net
                res = factCheckConverge' (i-1) net'
             in traceShowId $ fmap (caught++) res

        -- | casts factCheckStep to use Identity Activator
        factCheckStepAllAct :: NetworkData -> ([AS], NetworkData)
        factCheckStepAllAct net = adj $ runStateWriter factCheckStep net
        adj ((),s,w) = (w,s)


initializeMessages :: NetworkData -> NetworkData
initializeMessages network =
  network & networkMessages .~ Map.fromList messages
  where messages = do -- list monad
          i <- view networkAsNumbers network
          let Just agentData = view (networkAses . at i) network
          case view asQueryStrategy agentData of
            HonestAnswerQueries -> return (i, queryAllHops network i)
            ManipulatorAnswerQueries -> return (i, [])

queryAllHops :: NetworkData -> AS -> [(AS, Query)]
queryAllHops network agent =
  let Just agentState = view (networkAses . at agent) network
      hopQueries = do -- (List monad)
        (dest, path) <- Map.toList $ view asForwardTable agentState
        (manip, nextHop) <- tailSafe $ zip path (tailSafe path)
        return $ Query manip nextHop dest
   in fmap (agent,) hopQueries

factCheckStep
  :: (Activator m, MonadWriter [AS] m, MonadState NetworkData m) => m ()
factCheckStep = do
  activatedAgents <- activate =<< use networkAsNumbers
  mapM_ factCheckActivation activatedAgents

-- | The main workhorse
factCheckActivation :: (MonadWriter [AS] m, MonadState NetworkData m) => AS -> m ()
factCheckActivation agent = do
  Just previousQueries <- preuse $ networkAses . ix agent . asPreviousQueries
  Just messages <- preuse $ networkMessages . ix agent
  assign (networkMessages . at agent) (Just [])
  let updatedQueries = union previousQueries (fmap snd messages)
  assign (networkAses . ix agent . asPreviousQueries) updatedQueries

  forwards <- concat <$> mapM (uncurry $ processMessage agent) messages
  mapM_ (uncurry $ sendMessage agent) forwards

  return ()

sendMessage :: MonadState NetworkData m => AS -> AS -> Query -> m ()
sendMessage from to query = networkMessages . ix to %= addQuery
  where addQuery messages = (from,query) : messages

-- | Return value is messages to forward
processMessage :: (MonadWriter [AS] m, MonadState NetworkData m) =>
  AS -> AS -> Query -> m [(AS, Query)]
processMessage agent sender query = do
  queryAnswer <- factCheckAnswerQuery agent query
  case queryAnswer of
    Just True -> return []
    Just False -> tell [view queryManipulator query] >> return []
    Nothing -> do
      Just neighbors <- use (networkTopology . at agent)
      let forwardingNeighbors
            = (neighbors \\ [view queryManipulator query]) \\ [sender]
      return $ fmap (,query) forwardingNeighbors
      -- ^ collect the neighbors in this list

-- | Nothing means ``I don't know''
factCheckAnswerQuery :: MonadState NetworkData m => AS -> Query -> m (Maybe Bool)
factCheckAnswerQuery agent query = do
  forwardingHere <- asesForwardingHere agent (view queryDestination query)
  let manip = view queryManipulator query
  if agent == view queryNextHop query
    then return . Just $ manip `elem` forwardingHere
      -- ^ Here, an agent truthfully forwarding to you gets "cleared"
    else if manip `elem` forwardingHere
      then return $ Just False
      else return Nothing

--------------------------------------------------------------------------------

-- | If an AS is ``cleared'' (we believe he's telling the truth)
-- or ``caught'' (punished with -infinite utility)
-- this function can remove all queries about that AS.
dontWorryAbout :: NetworkData -> AS -> NetworkData
dontWorryAbout = undefined

asesForwardingHere :: forall m. MonadState NetworkData m => AS -> AS -> m [AS]
asesForwardingHere agent dest = do
  let forwardsToAgent :: AS -> m Bool
      forwardsToAgent n = do
        Just as <- use (networkAses . at n)
        let Just pathOfN = view (asForwardTable . at dest) as
            forwardsByTable = case pathOfN of
              (_:nextHop:_) -> nextHop == agent
              _ -> False
            forwardsByManip = case view asForwardStrategy as of
              HonestForwardByTable -> False
              ManipulatorForwardHardcoded fwds -> n `elem` fwds
        return $ forwardsByTable || forwardsByManip

  asNums <- use networkAsNumbers
  filterM forwardsToAgent asNums

  -- do
  -- use (
  -- let forwardsToAgent state =
  --       case view (asForwardTable . at dest) state of
  --         Just (_:nextHop:_) -> nextHop == agent
  --         _ -> False
  --  in Map.keys $ Map.filter forwardsToAgent (view networkAses network)
