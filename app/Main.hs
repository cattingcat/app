module Main (main) where

import Lib (someFunc)
import DbExamples (showOldPeople)
import ServantExamples(runServer)


main :: IO ()
main = do
  _ <- someFunc
  _ <- showOldPeople
  _ <- runServer
  pure ()