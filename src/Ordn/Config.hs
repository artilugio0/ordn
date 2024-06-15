{-# LANGUAGE DeriveGeneric #-}
module Ordn.Config where

import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Aeson as Aeson
import GHC.Generics
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (Reader, ask)

import qualified Ordn.Date as Date
import Ordn.PeriodicLog
import Ordn.Document

data Config = Config
  { dailyDir :: Maybe FilePath
  , documentsDir :: Maybe FilePath
  , templatesDir :: Maybe FilePath
  , dailyTemplateName :: Maybe FilePath
  , defaultTemplateName :: Maybe FilePath
  , periodicTemplateName :: Maybe FilePath
  , periodicLogPath :: Maybe FilePath
  } deriving (Generic, Show)

instance Aeson.ToJSON Config where
  -- Generic

instance Aeson.FromJSON Config where
  -- Generic

data Environment = Environment
  { config :: Config
  , periodicLog :: [PeriodicLogEntry ChecklistItem]
  , today :: Date.Date
  , timestamp :: Integer
  } deriving Show


defaultConfig :: Config
defaultConfig = Config
  { dailyDir = Just defaultDailyDir
  , documentsDir = Just defaultDocumentsDir
  , templatesDir = Just defaultTemplatesDir
  , dailyTemplateName = Just defaultDailyTemplateName
  , defaultTemplateName = Just defaultDefaultTemplateName
  , periodicTemplateName = Just defaultPeriodicTemplateName
  , periodicLogPath = Just defaultPeriodicLogPath
  }


configFilePath :: FilePath
configFilePath = "./config.json"


loadConfig :: IO Config
loadConfig = do
  configFileExists <- Dir.doesFileExist configFilePath
  if configFileExists
  then do
    content <- Char8.readFile configFilePath
    pure $ fromMaybe defaultConfig $ Aeson.decode content
  else pure defaultConfig


defaultEnvironment :: Environment
defaultEnvironment = Environment
  { config = defaultConfig
  , periodicLog = []
  , today = Date.Date 0 0 0
  , timestamp = 0
  }

loadEnvironment :: IO Environment
loadEnvironment = do
  cfg <- loadConfig

  let pLogPath = fromMaybe defaultPeriodicLogPath $ periodicLogPath cfg
  pLogFileExists <- Dir.doesFileExist pLogPath

  if not pLogFileExists
    then writeFile pLogPath "[]"
    else pure ()

  periodicLogFileContent <- Char8.readFile pLogPath
  date <- Date.getDate
  timestamp' <- Date.getTimestamp

  let
    pLog = fromMaybe [] $ Aeson.decode periodicLogFileContent

  pure $ Environment { config = cfg ,today = date, timestamp = timestamp', periodicLog = pLog}

defaultDailyDir :: String
defaultDailyDir = "./daily/"

defaultDocumentsDir :: String
defaultDocumentsDir = "./docs/"

defaultTemplatesDir :: String
defaultTemplatesDir = "./templates/"

defaultDailyTemplateName :: String
defaultDailyTemplateName = "daily.md"

defaultDefaultTemplateName :: String
defaultDefaultTemplateName = "default.md"

defaultPeriodicTemplateName :: String
defaultPeriodicTemplateName = "periodic.md"

defaultPeriodicLogPath :: String
defaultPeriodicLogPath = "./.state"

templatePath :: String -> Reader Environment FilePath
templatePath name = do
  dir <- getTemplatesDir
  pure $ dir ++ name


dailyTemplatePath :: Reader Environment FilePath
dailyTemplatePath = do
  templateName <- getDailyTemplateName
  templatePath templateName


periodicTemplatePath :: Reader Environment FilePath
periodicTemplatePath = do
  templateName <- getPeriodicTemplateName
  templatePath templateName


getDailyDir :: Reader Environment FilePath
getDailyDir = do
  env <- ask
  pure $ fromMaybe defaultDailyDir (dailyDir $ config env)


getDocumentsDir :: Reader Environment FilePath
getDocumentsDir = do
  env <- ask
  pure $ fromMaybe defaultDocumentsDir (documentsDir $ config env)


getTemplatesDir :: Reader Environment FilePath
getTemplatesDir = do
  env <- ask
  pure $ fromMaybe defaultTemplatesDir (templatesDir $ config env)


getDailyTemplateName :: Reader Environment FilePath
getDailyTemplateName = do
  env <- ask
  pure $ fromMaybe defaultDailyTemplateName (dailyTemplateName $ config env)


getDefaultTemplateName :: Reader Environment FilePath
getDefaultTemplateName = do
  env <- ask
  pure $ fromMaybe defaultDefaultTemplateName (defaultTemplateName $ config env)


getPeriodicTemplateName :: Reader Environment FilePath
getPeriodicTemplateName = do
  env <- ask
  pure $ fromMaybe defaultPeriodicTemplateName (periodicTemplateName $ config env)


getPeriodicLogPath :: Reader Environment FilePath
getPeriodicLogPath = do
  env <- ask
  pure $ fromMaybe defaultPeriodicLogPath (periodicLogPath $ config env)


getLookupTable :: Reader Environment [(String, String)]
getLookupTable = do
  dateStr <- showDate <$> today <$> ask
  pure
    [ ("date", dateStr)
    ]
