module WebApi.MonadDb (
  MonadDb(..),

) where

import Hasql.Pool (UsageError)
import Hasql.Session

class Monad m => MonadDb m where
  runSession :: Session a -> m (Either UsageError a)
