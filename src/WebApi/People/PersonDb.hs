{-# LANGUAGE QuasiQuotes #-}

module WebApi.People.PersonDb (
  showOldPeople,
  showOldPeople',
  retrieveOldPeople,
  retrieveOldPeople'
) where

import Data.Int
import Data.Either
import Data.Text
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import qualified Data.Vector as V

import App (StartupConfig(..), App)
import WebApi.People.Data
import Control.Monad.Reader


showOldPeople :: App ()
showOldPeople = do
  r <- ask
  liftIO $ showOldPeople' r

showOldPeople' :: StartupConfig -> IO ()
showOldPeople' (StartupConfig q w e r t) = do
  conn <- Connection.acquire (Connection.settings q w e r t)
  case conn of
    Left err -> print err
    Right connection -> do
      result <- Session.run (selectOlderThanSession 15) connection
      mapM_ putStrLn ((\(n, a) -> unpack n ++ " " ++ show a) <$> fromRight undefined result )


retrieveOldPeople :: App [Person]
retrieveOldPeople = do
  r <- ask
  liftIO $ retrieveOldPeople' r

retrieveOldPeople' :: StartupConfig -> IO [Person]
retrieveOldPeople' (StartupConfig q w e r t) = do
  conn <- Connection.acquire (Connection.settings q w e r t)
  case conn of
    Left err -> error $ "err 1: " ++ show err
    Right connection -> do
      result <- Session.run (selectOlderThanSession 15) connection
      case result of
        Left _ -> error "err 2"
        Right v -> pure $ fmap toPerson (V.toList v)
          where
            toPerson :: (Text, Int64) -> Person
            toPerson (name, age) = MkPerson name (fromIntegral age)


selectOlderThanSession :: Int64 -> Session (V.Vector (Text, Int64))
selectOlderThanSession minAge = Session.statement minAge selectOlderStatement
  where
    selectOlderStatement :: Statement Int64 (V.Vector (Text, Int64))
    selectOlderStatement =
      [TH.vectorStatement|
        select
          p."Name" :: text,
          p."Age"  :: int8
          from person p
          where p."Age" > $1 :: int8
        |]