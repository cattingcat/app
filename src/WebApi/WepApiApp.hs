module WebApi.WepApiApp (
  runServer
) where


import Data.Proxy
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BS
import qualified Servant as S
import Servant.API
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as WR
import App (StartupConfig(..), App)
import Control.Monad.Except
import Control.Monad.Reader

import qualified WebApi.People.PersonApi as P
import qualified WebApi.Recipe.RecipeApi as R

import Hasql.Pool (Pool, acquire)
import qualified Hasql.Connection as C
import WebApi.H
import Data.Kind


type ServerAPI = P.UserAPI :<|> R.RecipeAPI

server :: StartupConfig -> Pool -> S.Server ServerAPI
server c p = P.server c :<|> recipeServer p



recipeServer :: Pool -> S.Server R.RecipeAPI
recipeServer p = q (Proxy @R.RecipeAPI) p (R.server @H)

-- | Because of PolyKinds @api@ is too polymorphic, so we have to add some restrictions
q :: forall (api :: Type) . (S.HasServer api '[]) => Proxy api -> Pool -> S.ServerT api H -> S.Server api
q p pool = S.hoistServer p (func pool)

func :: Pool -> H a -> S.Handler a
func pool (H h) = S.Handler $ do
  eith <- liftIO $ runExceptT $ runReaderT h pool
  case eith of
    Left err -> let
      errString = "Db error: " <> pack (show err)
      in throwError $ S.ServerError 500 ""  (BS.fromStrict $ encodeUtf8 errString) []
    Right a -> pure a




-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: StartupConfig -> Pool -> W.Application
app c p = S.serve (Proxy @ServerAPI) (server c p)


runServer :: App ()
runServer = do
  conf@StartupConfig{..} <- ask
  pool <- liftIO $ acquire (5, 600, C.settings host port user pass dbName)
  liftIO $ WR.run 8081 (app conf pool)