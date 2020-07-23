module ServantExamples (
  runServer
) where

import Data.Proxy
import Control.Monad.Except
import qualified Servant as S
import Servant.API
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as WR
import Models
import DbExamples(retrieveOldPeople')
import App.App (StartupConfig(..), App)
import Control.Monad.Reader


type UserAPI = "persons" :> Get '[JSON] [Person]

server :: StartupConfig -> S.Server UserAPI
server c = S.Handler users1
  where
    users1 :: ExceptT S.ServerError IO [Person]
    users1 = do
      olds <- liftIO $ retrieveOldPeople' c
      pure $ Person "q" 4 : olds

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: StartupConfig -> W.Application
app c = S.serve (Proxy @UserAPI) (server c)


runServer :: App ()
runServer = do
  r <- ask
  liftIO $ WR.run 8081 (app r)