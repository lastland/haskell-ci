module Main where

import Control.Lens

import BuildConfig
import WorkflowConfig
import Example

main :: IO ()
main = do
  w <- buildWorkflowConfig workflow
  putStrLn $ show w
  putStrLn $ show $ length $ w ^. jobs
