module Main
  ( main
  ) where

import           Docker
import           RIO

main :: IO ()
main = do
  cId <- createContainer $ CreateContainerOptions (Image "ubuntu")
  startContainer cId
