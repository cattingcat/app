module WebApi.H (
  H(..)
) where

import Control.Monad.Except
import Control.Monad.Reader

import Hasql.Pool (use, Pool, UsageError)

import WebApi.MonadDb



newtype H a = H { runH :: ReaderT Pool (ExceptT UsageError IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Pool, MonadIO)

instance MonadDb H where
  runSession session = do
    p <- ask
    liftIO $ use p session
