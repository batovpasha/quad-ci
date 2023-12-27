import           Core
import           Docker
import           Prelude
import           RIO
import qualified RIO.Map              as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
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

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ -> pure newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess docker = do
  result <- runBuild docker testBuild
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main =
  hspec $ do
    let dockerService = createService
    beforeAll cleanupDocker $ describe "Quad CI" $ do
      it "should run a build (success)" do testRunSuccess dockerService
