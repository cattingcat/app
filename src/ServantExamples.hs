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
import DbExamples(retrieveOldPeople)


type UserAPI = "persons" :> Get '[JSON] [Person]

server :: S.Server UserAPI
server = S.Handler users1
  where
    users1 :: ExceptT S.ServerError IO [Person]
    users1 = do
      olds <- liftIO retrieveOldPeople
      pure $ Person "q" 4 : olds



-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: W.Application
app = S.serve (Proxy @UserAPI) server


runServer :: IO ()
runServer = WR.run 8081 app