module Docker where

import qualified Data.Aeson          as Aeson
import qualified Network.HTTP.Simple as HTTP
import           RIO
import qualified Socket

data CreateContainerOptions =
  CreateContainerOptions
    { image :: Image
    }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let body = Aeson.Null -- TODO figure out actual request body
  let req =
        HTTP.defaultRequest & HTTP.setRequestManager manager &
        HTTP.setRequestPath "/v1.40/containers/create" &
        HTTP.setRequestMethod "POST" &
        HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
    -- Dump the response to stdout to check what we're getting back.
  traceShowIO res

newtype Image =
  Image Text
  deriving (Eq, Show)

newtype ContainerExitCode =
  ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image
