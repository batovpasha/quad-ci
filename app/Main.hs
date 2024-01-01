module Main
  ( main
  ) where

import qualified Docker
import           RIO

main :: IO ()
main = do
  dockerService <- Docker.createService
  cId <-
    dockerService.createContainer $
    Docker.CreateContainerOptions
      {image = (Docker.Image "ubuntu"), script = "echo 'ok'"}
  dockerService.startContainer cId
  dockerService.containerStatus cId
  return ()
