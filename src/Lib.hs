module Lib (
  showIntro
) where

import App.App (App)
import Control.Monad.Reader


showIntro :: App ()
showIntro = do
  conf <- ask
  liftIO $ putStrLn "Startup config:"
  liftIO $ print conf

