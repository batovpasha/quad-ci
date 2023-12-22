module Core where

import           RIO
import qualified RIO.List as List
import qualified RIO.Map  as Map

data Pipeline =
  Pipeline
    { steps :: NonEmpty Step
    }
  deriving (Eq, Show)

data Step =
  Step
    { name     :: StepName
    , commands :: NonEmpty Text
    , image    :: Image
    }
  deriving (Eq, Show)

data Build =
  Build
    { pipeline       :: Pipeline
    , state          :: BuildState
    , completedSteps :: Map StepName StepResult
    }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

data BuildRunningState =
  BuildRunningState
    { step :: StepName
    }
  deriving (Eq, Show)

newtype StepName =
  StepName Text
  deriving (Eq, Show, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

newtype ContainerExitCode =
  ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

newtype Image =
  Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
           Just step -> Right step
           Nothing   -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result -> pure $ build {state = BuildFinished result}
        Right step -> do
          let s = BuildRunningState {step = step.name}
          pure $ build {state = BuildRunning s}
    BuildRunning state
      -- We'll assume the container exited with a 0 status code.
     -> do
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit
      pure
        build
          { state = BuildReady
          , completedSteps =
              Map.insert state.step result build.completedSteps
          }
    BuildFinished _ -> undefined
