module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM

import           Core
import qualified JobHandler
import           RIO
import qualified RIO.List               as List
import qualified RIO.Map                as Map

data State = State
  { jobs      :: Map BuildNumber JobHandler.Job
  , nextBuild :: Int
  } deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <- STM.newTVarIO State {jobs = mempty, nextBuild = 1}
  pure
    JobHandler.Service
      { queueJob =
          \pipeline ->
            STM.atomically do STM.stateTVar state $ queueJob_ pipeline
      , findJob =
          \number ->
            STM.atomically $ do
              s <- STM.readTVar state
              pure $ findJob_ number s
      , dispatchCmd = STM.atomically do STM.stateTVar state dispatchCmd_
      , processMsg = \_ -> undefined
      }

queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state = (number, updatedState)
  where
    number = BuildNumber state.nextBuild
    job = JobHandler.Job {pipeline = pipeline, state = JobHandler.JobQueued}
    updatedState =
      state
        { jobs = Map.insert number job state.jobs
        , nextBuild = state.nextBuild + 1
        }

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state = Map.lookup number state.jobs

dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state =
  case List.find isQueued $ Map.toList state.jobs of
    Just (number, job) ->
      let updatedJob = job{state = JobHandler.JobAssigned}
          updatedState = Map.insert number updatedJob state.jobs
          cmd = Just $ Agent.StartBuild number job.pipeline
       in (cmd, state{jobs = updatedState})
    _ -> (Nothing, state)
  where
    isQueued (_, job) = job.state == JobHandler.JobQueued
