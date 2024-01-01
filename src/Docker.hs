module Docker where

import           Data.Aeson            ((.:))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson.Types
import           Data.ByteString.Char8 (putStrLn)
import qualified Network.HTTP.Simple   as HTTP
import           RIO
import qualified Socket

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId
  , startContainer  :: ContainerId -> IO ()
  , containerStatus :: ContainerId -> IO ContainerStatus
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
      }

data CreateContainerOptions = CreateContainerOptions
  { image :: Image
  }

newtype ContainerId =
  ContainerId Text
  deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  let imageName = imageToText options.image
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON imageName)
          , ("Tty", Aeson.toJSON True)
          , ("Labels", Aeson.object [("quad", "")])
          , ("Cmd", "echo hello")
          , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
  let req =
        makeReq "/containers/create" & HTTP.setRequestMethod "POST" &
        HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  let parser =
        Aeson.withObject "create-container" $ \o -> do
          cId <- o .: "Id"
          pure $ ContainerId cId
  parseResponse res parser

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
  parseResponse res parser

parseResponse ::
     HTTP.Response ByteString -> (Aeson.Value -> Aeson.Types.Parser a) -> IO a
parseResponse res parser = do
  let body = HTTP.getResponseBody res
  -- TODO: log action name to have context: createContainer, startContainer, containerStatus
  putStrLn $ "Response body:\n" <> body
  let result = do
        value <- Aeson.eitherDecodeStrict body
        Aeson.Types.parseEither parser value
  case result of
    Left e       -> throwString e
    Right status -> pure status

newtype Image =
  Image Text
  deriving (Eq, Show)

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
imageToText (Image imageName) = imageName
