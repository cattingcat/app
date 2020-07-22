module Main (main) where

import Lib (someFunc)
import DbExamples (showOldPeople)


main :: IO ()
main = do
  _ <- someFunc
  _ <- showOldPeople
  pure ()