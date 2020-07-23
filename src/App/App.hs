module App.App (
  StartupConfig(..),
  StartupError,
  App,

  loadConfig
) where

import Data.Word
import Data.ByteString hiding (putStrLn)
import Control.Monad.Reader
import Control.Monad.Except
import qualified System.Envy as E
import Options.Applicative
import Text.Read (readMaybe)


data StartupConfig = StartupConfig
  { host :: ByteString
  , port :: Word16
  , user :: ByteString
  , pass :: ByteString
  , dbName :: ByteString
  } deriving stock Show

data StartupError = DbAddressNotSpecified
  deriving stock Show

type App a = ReaderT StartupConfig IO a

loadConfig :: ExceptT StartupError IO StartupConfig
loadConfig = do
  cliOpts <- liftIO readCliArgs
  envOpts <- liftIO readEnvArgs
  let
    foo :: (DBConfig -> Maybe a) -> DBConfig -> DBConfig -> Maybe a -> Maybe a
    foo f a b o = f a <|> f b <|> o
    opts = DBConfig
      (foo cHost cliOpts envOpts (Just "51.158.130.90"))
      (foo cPort cliOpts envOpts (Just 10997))
      (foo cUser cliOpts envOpts (Just "admin"))
      (foo cPass cliOpts envOpts (Just "F%HzxKnu9|X7ec24Z)"))
      (foo cDbName cliOpts envOpts (Just "rdb"))
  case opts of
    DBConfig (Just h) (Just p) (Just u) (Just ps) (Just n) -> pure $ StartupConfig h p u ps n
    _ -> throwError DbAddressNotSpecified




data DBConfig = DBConfig
  { cHost   :: Maybe ByteString
  , cPort   :: Maybe Word16
  , cUser   :: Maybe ByteString
  , cPass   :: Maybe ByteString
  , cDbName :: Maybe ByteString
  } deriving stock Show

dbHostParser, dbUserParser, dbPassParser, dbNameParser :: Parser (Maybe ByteString)
dbHostParser = optional $ strOption (long "DB_HOST" <> help "db host")
dbUserParser = optional $ strOption (long "DB_USER" <> help "db user")
dbPassParser = optional $ strOption (long "DB_PASS" <> help "db password")
dbNameParser = optional $ strOption (long "DB_NAME" <> help "db name")

dbPortParser :: Parser (Maybe Word16)
dbPortParser = (>>= tryParse) <$> optional (strOption (long "DB_PORT" <> help "db port"))
  where
    tryParse :: String -> Maybe Word16
    tryParse = readMaybe

dbConfigParser = DBConfig <$> dbHostParser <*> dbPortParser <*> dbUserParser <*> dbPassParser <*> dbNameParser

appOpts :: ParserInfo DBConfig
appOpts = info (dbConfigParser <**> helper) fullDesc

readCliArgs :: IO DBConfig
readCliArgs = execParser appOpts

readEnvArgs :: IO DBConfig
readEnvArgs = do
  eith <- E.runEnv $ DBConfig
    <$> E.envMaybe "DB_HOST"
    <*> E.envMaybe "DB_PORT"
    <*> E.envMaybe "DB_USER"
    <*> E.envMaybe "DB_PASS"
    <*> E.envMaybe "DB_NAME"
  pure $ case eith of
    Left  _ -> DBConfig Nothing Nothing Nothing Nothing Nothing
    Right r -> r
