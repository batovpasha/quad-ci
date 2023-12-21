module Core where

import           RIO

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
    { pipeline :: Pipeline
    , state    :: BuildState
    }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName =
  StepName Text
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

newtype Image =
  Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image
