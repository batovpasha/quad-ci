module Docker where

import           Data.Aeson            ((.:))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson.Types
import           Data.ByteString.Char8 (putStrLn)
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Simple   as HTTP
import           RIO
import qualified RIO.Text              as Text
import qualified RIO.Text.Partial      as Text.Partial
import qualified Socket

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId
  , startContainer  :: ContainerId -> IO ()
  , containerStatus :: ContainerId -> IO ContainerStatus
  , createVolume    :: IO Volume
  , fetchLogs       :: FetchLogsOptions -> IO ByteString
  , pullImage       :: Image -> IO ()
  }

type RequestBuilder = Text -> HTTP.Request

createService :: IO Service
createService = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest &
        HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path) &
        HTTP.setRequestManager manager
  pure
    Service
      { createContainer = createContainer_ makeReq
      , startContainer = startContainer_ makeReq
      , containerStatus = containerStatus_ makeReq
      , createVolume = createVolume_ makeReq
      , fetchLogs = fetchLogs_ makeReq
      , pullImage = pullImage_ makeReq
      }

data CreateContainerOptions = CreateContainerOptions
  { image  :: Image
  , script :: Text
  , volume :: Volume
  }

newtype ContainerId =
  ContainerId Text
  deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  let imageName = imageToText options.image
  let workDir = "/app"
  let bind = volumeToText options.volume <> ":" <> workDir
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON imageName)
          , ("Tty", Aeson.toJSON True)
          , ("Labels", Aeson.object [("quad", "")])
          , ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh")
          , ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script])
          , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          , ("WorkingDir", Aeson.toJSON workDir)
          , ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])])
          ]
  let path = "/containers/create"
  let req =
        makeReq path & HTTP.setRequestMethod "POST" &
        HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  let parser =
        Aeson.withObject "create-container" $ \o -> do
          cId <- o .: "Id"
          pure $ ContainerId cId
  parseResponse res parser path

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq containerId = do
  let path = mconcat ["/containers/", containerIdToText containerId, "/start"]
  let req = makeReq path & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq containerId = do
  let parser =
        Aeson.withObject "container-inspect" $ \o -> do
          state <- o .: "State"
          status <- state .: "Status"
          case status of
            "running" -> pure ContainerRunning
            "exited" -> do
              code <- state .: "ExitCode"
              pure $ ContainerExited (ContainerExitCode code)
            other -> pure $ ContainerOther other
  let path = mconcat ["/containers/", containerIdToText containerId, "/json"]
  let req = makeReq path
  res <- HTTP.httpBS req
  parseResponse res parser path

parseResponse ::
     HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> Text -- ^ path is only for debugging purposes
  -> IO a
parseResponse res parser path = do
  let body = HTTP.getResponseBody res
  -- TODO: pretty print the json body
  putStrLn $
    "Response body from the " <> encodeUtf8 path <> " endpoint:\n" <> body
  let result = do
        value <- Aeson.eitherDecodeStrict body
        Aeson.Types.parseEither parser value
  case result of
    Left e       -> throwString e
    Right status -> pure status

data Image = Image
  { name :: Text
  , tag  :: Text
  } deriving (Eq, Show)

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" $ \image -> do
    case Text.Partial.splitOn ":" image of
      [name] -> pure $ Image { name = name, tag = "latest" }
      [name, tag] -> pure $ Image { name = name, tag = tag }
      _ -> fail $ "Image has too many colons " <> Text.unpack image

newtype ContainerExitCode =
  ContainerExitCode Int
  deriving (Eq, Show)

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText image = image.name <> ":" <> image.tag

newtype Volume =
  Volume Text
  deriving (Eq, Show)

volumeToText :: Volume -> Text
volumeToText (Volume v) = v

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body = Aeson.object [("Labels", Aeson.object [("quad", "")])]
  let path = "/volumes/create"
  let req =
        makeReq path & HTTP.setRequestMethod "POST" &
        HTTP.setRequestBodyJSON body
  let parser =
        Aeson.withObject "create-volume" $ \o -> do
          name <- o .: "Name"
          pure $ Volume name
  res <- HTTP.httpBS req
  parseResponse res parser path

data FetchLogsOptions = FetchLogsOptions
  { containerId :: ContainerId
  , since       :: Time.POSIXTime
  , until       :: Time.POSIXTime
  }

fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ makeReq options = do
  let timestampToText t = tshow (round t :: Int)
  let url =
        "/containers/" <>
        containerIdToText options.containerId <>
        "/logs?stdout=true&stderr=true&since=" <>
        timestampToText options.since <>
        "&until=" <> timestampToText options.until
  res <- HTTP.httpBS $ makeReq url
  pure $ HTTP.getResponseBody res

pullImage_ :: RequestBuilder -> Image -> IO ()
pullImage_ makeReq image = do
  let url =
        "/images/create?tag=" <> image.tag <> "&fromImage=" <> image.name
  let req = makeReq url & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req
