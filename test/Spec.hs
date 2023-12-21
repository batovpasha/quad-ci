import           Core
import           Prelude
import           RIO
import           Test.Hspec

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step {name = StepName name, image = Image image, commands = commands}

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {steps = steps}

-- Test values
testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild = Build {pipeline = testPipeline, state = BuildReady}

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
                      [ Step
                          { name = StepName "First step"
                          , commands = ["date"]
                          , image = Image "ubuntu"
                          }
                      , Step
                          { name = StepName "Second step"
                          , commands = ["uname -r"]
                          , image = Image "ubuntu"
                          }
                      ]
                  }
            , state = BuildReady
            }
