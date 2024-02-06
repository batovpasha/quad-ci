import qualified Agent
import qualified Control.Concurrent.Async as Async
import           Core
import qualified Data.Yaml                as Yaml
import           Docker
import qualified JobHandler
import qualified JobHandler.Memory
import           Prelude
import           RIO
import qualified RIO.ByteString           as ByteString
import qualified RIO.Map                  as Map
import qualified RIO.NonEmpty.Partial     as NonEmpty.Partial
import qualified RIO.Set                  as Set
import qualified Runner
import qualified Server
import qualified System.Process.Typed     as Process
import           Test.Hspec

cleanupDocker :: IO ()
cleanupDocker =
  void $ do
    Process.readProcessStdout
      "docker rm -f $(docker ps -aq --filter \"label=quad\")"
    Process.readProcessStdout
      "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Docker.Image {name = image, tag = "latest"}
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline {steps = NonEmpty.Partial.fromList steps}

emptyHooks :: Runner.Hooks
emptyHooks = Runner.Hooks {logCollected = \_ -> pure ()}

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
    makePipeline
      [ makeStep "First step" "ubuntu" ["date"]
      , makeStep "Second step" "ubuntu" ["uname -r"]
      ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <-
    runner.prepareBuild $
    makePipeline [makeStep "Should fail" "ubuntu" ["exit 1"]]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe`
    [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  build <-
    runner.prepareBuild $
    makePipeline
      [ makeStep "Create file" "ubuntu" ["echo hello > text.txt"]
      , makeStep "Read file" "ubuntu" ["cat text.txt"]
      ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_, "") -> pure ()
            _       -> modifyMVar_ expected (pure.Set.delete word)
  let hooks = Runner.Hooks {logCollected = onLog}
  build <-
    runner.prepareBuild $
    makePipeline
      [ makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
      , makeStep "Echo Linux" "ubuntu" ["uname -s"]
      ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker rmi -f busybox"
  build <-
    runner.prepareBuild $
    makePipeline [makeStep "First step" "busybox" ["date"]]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent runner = do
  handler <- JobHandler.Memory.createService
  serverThread <- Async.async do Server.run (Server.Config 9000) handler
  Async.link serverThread
  agentThread <-
    Async.async do Agent.run (Agent.Config "http://localhost:9000") runner
  Async.link agentThread
  let pipeline =
        makePipeline
          [makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]]
  number <- handler.queueJob pipeline
  checkBuild handler number
  Async.cancel serverThread
  Async.cancel agentThread

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
  where
    loop = do
      Just job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            BuildFinished s -> s `shouldBe` BuildSucceeded
            _               -> loop
        _ -> loop

main :: IO ()
main =
  hspec $ do
    docker <- runIO Docker.createService
    let runner = Runner.createService docker
    beforeAll cleanupDocker $
      describe "Quad CI" $ do
        it "should run a build (success)" do testRunSuccess runner
        it "should run a build (failure)" do testRunFailure runner
        it "should share workspace between steps" do testSharedWorkspace runner
        it "should collect logs" do testLogCollection runner
        it "should pull image" do testImagePull runner
        it "should decode yaml config into pipeline" do testYamlDecoding runner
        fit "should run server and agent" do testServerAndAgent runner
