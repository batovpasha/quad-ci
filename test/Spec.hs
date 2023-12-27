import           Core
import           Docker
import           Prelude
import           RIO
import qualified RIO.Map              as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Runner
import qualified System.Process.Typed as Process
import           Test.Hspec

cleanupDocker :: IO ()
cleanupDocker =
  void $ do
    Process.readProcessStdout
      "docker rm -f $(docker ps -aq --filter \"label=quad\")"

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Docker.Image image
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {steps = NonEmpty.Partial.fromList steps}

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
    makePipeline
      [ makeStep "First step" "ubuntu" ["date"]
      , makeStep "Second step" "ubuntu" ["uname -r"]
      ]
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main =
  hspec $ do
    let dockerService = Docker.createService
    let runnerService = Runner.createService dockerService
    beforeAll cleanupDocker $ describe "Quad CI" $ do
      it "should run a build (success)" do testRunSuccess runnerService
