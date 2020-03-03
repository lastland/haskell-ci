{-# LANGUAGE ApplicativeDo #-}
module Example where

import Control.Lens

import Executor
import Persistance
import Job
import Resumption
import Workflow

doc :: Executor
doc = docker "ubuntu-1604:201903-01"

job1 :: Job PersistanceHandle
job1 = Job "job1" doc js1
  where js1 :: Steps PersistanceHandle
        js1 = do
          runStep "echo 'Hello World' > file1.txt"
          persistToWorkspace (Workspace "." ["file1.txt"])

job2 :: Job PersistanceHandle
job2 = Job "job2" doc js2
  where js2 :: Steps PersistanceHandle
        js2 = do
          runStep "echo 'Hello Universe' > file2.txt"
          persistToWorkspace (Workspace "." ["file2.txt"])
          
job3 :: PersistanceHandle -> PersistanceHandle -> Job DependencyToken
job3 p1 p2 = Job "job3" doc $ js3
  where js3 = do
          w1 <- attachWorkspace p1
          w2 <- attachWorkspace p2
          let allfiles = w1 ^. paths <> w2 ^. paths
          _ <- traverse (\p -> runStep $ "cat " ++ p) allfiles
          createDependency

job4 :: Job ()
job4 = Job "job4" doc $ do
  runStep "echo 'Hello A!'"

job5 :: DependencyToken -> Job ()
job5 _ = Job "job5" doc $ do
  runStep "echo 'Hello B!'"
  

workflow :: Workflow ()
workflow = Workflow "mywork" js
  where js = do
          p1 <- lift job1
          p2 <- lift job2
          t <- lift $ job3 p1 p2
          lift $ job4
          lift $ job5 t
