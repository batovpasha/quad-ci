module Main
  ( main
  ) where

import           Docker
import           RIO

main :: IO ()
main = do
  let dockerService = createService
  cId <- dockerService.createContainer $ CreateContainerOptions (Image "ubuntu")
  dockerService.startContainer cId
