import           Core
import           Prelude
import           RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import           Test.Hspec

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Image image
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {steps = NonEmpty.Partial.fromList steps}

-- Test values
testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild =
  Build {pipeline = testPipeline, state = BuildReady, completedSteps = mempty}

main :: IO ()
main =
  hspec $ do
    describe "testBuild" $ do
      it "should be a valid build" $ do
        testBuild ==
          Build
            { pipeline =
                Pipeline
                  { steps =
                      NonEmpty.Partial.fromList
                        [ Step
                            { name = StepName "First step"
                            , commands = NonEmpty.Partial.fromList ["date"]
                            , image = Image "ubuntu"
                            }
                        , Step
                            { name = StepName "Second step"
                            , commands = NonEmpty.Partial.fromList ["uname -r"]
                            , image = Image "ubuntu"
                            }
                        ]
                  }
            , state = BuildReady
            , completedSteps = mempty
            }
