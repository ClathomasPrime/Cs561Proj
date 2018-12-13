{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module FactCheck where

import Safe
import Data.Maybe
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Safe.Foldable

import Types
import Dynamics

import Debug.Trace

-- Right now, I'll assume that a manipulator node forwards
-- traffic in exactly one direction, and thus if one node can
-- coroborate the next-hop announced by the potential manipulator,
-- then that hop is ``cleared'' and the other nodes won't need to keep checking.
-- I'll try to point out when this manifests in code.

-- | After bgp stabilizes, run the fact checking round
factCheckConverge :: NetworkData -> Maybe [AS]
factCheckConverge network = factCheckConverge' 20 (initializeMessages network)
  where factCheckConverge' :: Int -> NetworkData -> Maybe [AS]
        factCheckConverge' 0 _ = Nothing
        factCheckConverge' i net
          | emptyNetworkMessages net = Just []
          | otherwise =
            let (caught, net') = factCheckStepAllAct net
                res = factCheckConverge' (i-1) net'
             in fmap (caught++) res

initializeMessages :: NetworkData -> NetworkData
initializeMessages network =
  network { networkMessages = Map.fromList $
    do i <- networkAsNumbers network
       let Just (agentData, _) = Map.lookup i (networkAses network)
       case asQueryStrategy agentData of
         HonestAnswerQueries -> return (i, queryAllHops network i)
         ManipulatorAnswerQueries _ -> return (i, [])
  }

queryAllHops :: NetworkData -> AS -> [(AS, QueryMessage)]
queryAllHops network@(NetworkData{..}) agent =
  let Just (_, agentState) = Map.lookup agent networkAses
      hopQueries = do
        (dest, path) <- Map.toList $ asForwardTable agentState
        (manip, nextHop) <- tailSafe $ zip path (tailSafe path)
        return . QueryForward $ Query manip nextHop dest
   in fmap (agent,) hopQueries

-- ^ some initialization code, then etc.

factCheckStepAllAct :: NetworkData -> ([AS], NetworkData)
factCheckStepAllAct network@(NetworkData{..}) =
  foldl accum ([], network) networkAsNumbers
  where accum (caughtAses, net) agent =
          let (caught', net') = factCheckActivation net agent
           in (caught' ++ caughtAses, net')

-- | The main workhorse
factCheckActivation :: NetworkData -> AS -> ([AS], NetworkData)
factCheckActivation network@(NetworkData{..}) agent =
  let Just (_, agentState) = Map.lookup agent networkAses
      previousQueries = asPreviousQueries agentState
      Just messages = fmap (fmap snd) $ Map.lookup agent networkMessages
      -- ^ Currently, I just throw away the source of the message...
      updatedQueries = union previousQueries (fmap getQuery messages)
      updatedAses = Map.adjust adj agent networkAses
      adj (d,s) = (d, s { asPreviousQueries = updatedQueries })

      (caughtAses, forwardMessages) = foldl accum ([],[]) messages
      accum (caughtAses, forwardMsgs) queryMessage
        | getQuery queryMessage `elem` previousQueries
          = (caughtAses, forwardMsgs)
          -- ^ If you've already seen this query, ignore it
        | otherwise =
          case factCheckForwardMessage network agent queryMessage of
            Left False ->
              let manip = (queryManipulator . getQuery $ queryMessage)
               in (manip : caughtAses, forwardMsgs)
              -- ^ caught a liar. Add him to the caught list
              -- Note: right now, a manip may pop up in the list multiple times
            Left True -> (caughtAses, forwardMsgs)
              -- ^ verified something. Don't forward the question
            Right newMessages -> (caughtAses, newMessages ++ forwardMsgs)
              -- ^ not sure. Forward the question.

      clearConsideredMessages = Map.insert agent [] networkMessages
      ins queryMessage oldMessageList
        = (agent, queryMessage) : oldMessageList
        -- | null . filter ((==queryMessage) . snd) $ oldMessageList
        --   -- ^ If the neighbor hasn't seen this query yet,
        --   = (agent, queryMessage) : oldMessageList -- ^ Send it to them
        --   -- ^ This is where you keep track of the message comming from agent
        -- | otherwise = oldMessageList
      newMessages =
        foldr (\(neighbor, message) -> Map.adjust (ins message) neighbor)
        clearConsideredMessages forwardMessages

   in (caughtAses, network { networkAses = updatedAses,
        networkMessages = newMessages })

-- | Left b means ``I know, so I'll stop forwarding''
--   Right [..(x,m)..] means ``I don't know, so I'll send m to x''
factCheckForwardMessage
  :: NetworkData -> AS -> QueryMessage -> Either Bool [(AS, QueryMessage)]
factCheckForwardMessage network@(NetworkData{..}) agent (QueryForward query) =
  case factCheckAnswerQuery network agent query of
    Just b -> Left b
    Nothing -> Right $ fmap (,QueryForward query) nonManipNeighbors
      -- ^ collect the neighbors in this list
  where Just neighbors = Map.lookup agent networkTopology
        nonManipNeighbors = filter (/= queryManipulator query) neighbors

-- | Nothing means ``I don't know''
factCheckAnswerQuery :: NetworkData -> AS -> Query -> Maybe Bool
factCheckAnswerQuery network@(NetworkData{..}) agent query
  | agent == queryNextHop query
    = Just $ queryManipulator query `elem` forwardingHere
  | otherwise =
    if queryManipulator query `elem` forwardingHere
      then Just False
      else Nothing
  where forwardingHere = asesForwardingHere network agent (queryDestination query)

--------------------------------------------------------------------------------

-- | If an AS is ``cleared'' (we believe he's telling the truth)
-- or ``caught'' (punished with -infinite utility)
-- this function can remove all queries about that AS.
dontWorryAbout :: NetworkData -> AS -> NetworkData
dontWorryAbout = undefined

asesForwardingHere :: NetworkData -> AS -> AS -> [AS]
asesForwardingHere network@(NetworkData{..}) agent dest =
  let forwardsToAgent (_,state) =
        case Map.lookup dest (asForwardTable state) of
          Just (_:nextHop:_) -> nextHop == agent
          _ -> False
   in Map.keys $ Map.filter forwardsToAgent networkAses
