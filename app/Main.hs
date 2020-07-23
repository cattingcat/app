module Main (main) where

import Lib (showIntro)
import DbExamples (showOldPeople)
import ServantExamples(runServer)

import App.App(
  loadConfig,
  App)
import Control.Monad.Reader
import Control.Monad.Except


main :: IO ()
main = do
  c <- runExceptT loadConfig
  case c of
    Left  err  -> print err
    Right conf -> runReaderT app conf

app :: App ()
app = do
  _ <- showIntro
  _ <- showOldPeople
  _ <- runServer
  pure ()