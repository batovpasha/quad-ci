module Main
  ( main
  ) where

import           Docker
import           RIO

main :: IO ()
main = do
  dockerService <- createService
  cId <- dockerService.createContainer $ CreateContainerOptions (Image "ubuntu")
  dockerService.startContainer cId
