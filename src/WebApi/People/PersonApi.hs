module WebApi.People.PersonApi (
  UserAPI,
  server
) where

import Control.Monad.Except
import qualified Servant as S
import Servant.API

import WebApi.People.Data
import WebApi.People.PersonDb
import App


type UserAPI = "persons" :> Get '[JSON] [Person]

server :: StartupConfig -> S.Server UserAPI
server c = S.Handler users1
  where
    users1 :: ExceptT S.ServerError IO [Person]
    users1 = do
      olds <- liftIO $ retrieveOldPeople' c
      pure $ MkPerson "q" 4 : olds