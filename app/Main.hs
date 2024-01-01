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
    Docker.CreateContainerOptions (Docker.Image "ubuntu")
  dockerService.startContainer cId
  dockerService.containerStatus cId
  return ()
