module Lib (
  showIntro
) where

import Control.Monad.Reader
import App


showIntro :: App ()
showIntro = do
  conf <- ask
  liftIO $ putStrLn "Startup config:"
  liftIO $ print conf

