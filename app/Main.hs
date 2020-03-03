module Main where

import Control.Lens

import Workflow
import Example

main :: IO ()
main = do
  w <- buildWorkflowConfig workflow
  putStrLn $ show w
