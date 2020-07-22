{-# LANGUAGE QuasiQuotes #-}

module DbExamples (
  showOldPeople
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

showOldPeople :: IO ()
showOldPeople = do
  conn <- Connection.acquire connectionSettings
  case conn of
    Left err -> print err
    Right connection -> do
      result <- Session.run (selectOlderThanSession 15) connection
      mapM_ putStrLn ((\(n, a) -> unpack n ++ " " ++ show a) <$> fromRight undefined result )

  where
    connectionSettings = Connection.settings
      "51.158.130.90" 10997
      "admin" "F%HzxKnu9|X7ec24Z)"
      "rdb"


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