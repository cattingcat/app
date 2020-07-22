{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import Data.Int
import Data.Either
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import Data.Text
import Data.Vector (Vector)

main :: IO ()
main = do
  _ <- someFunc
  conn <- Connection.acquire connectionSettings
  case conn of
    Right connection -> do
      result <- Session.run (selectSession 15) connection
      _ <- mapM_ putStrLn ((\(n, a) -> unpack n ++ " " ++ show a) <$> fromRight undefined result )
      pure ()
    Left err -> print err
  where
    connectionSettings = Connection.settings
      "51.158.130.90" 10997
      "admin" "F%HzxKnu9|X7ec24Z)"
      "rdb"


selectSession :: Int64 -> Session (Vector (Text, Int64))
selectSession minAge = Session.statement minAge selectStatement

selectStatement :: Statement Int64 (Vector (Text, Int64))
selectStatement =
  [TH.vectorStatement|
    select
      p."Name" :: text,
      p."Age"  :: int8
      from person p
      where p."Age" > $1 :: int8
    |]