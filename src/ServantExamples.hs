module ServantExamples (
  runServer
) where

import Data.Aeson (ToJSON)
import Data.Text
import Data.Proxy
import Control.Monad.Except
import GHC.Generics (Generic)

import qualified Servant as S
import Servant.API

import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as WR


type UserAPI = "persons" :> Get '[JSON] [Person]

data SortBy = Age | Name

data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)


users1 :: ExceptT S.ServerError IO [Person]
users1 = pure $
  [ Person "Isaac Newton"    372
  , Person "Albert Einstein" 136
  ]

server :: S.Server UserAPI
server = S.Handler users1


-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: W.Application
app1 = S.serve (Proxy @UserAPI) server


runServer :: IO ()
runServer = WR.run 8081 app1