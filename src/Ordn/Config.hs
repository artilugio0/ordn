{-# LANGUAGE DeriveGeneric #-}
module Ordn.Config where

import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import Data.Time.Calendar (toGregorian)
import GHC.Generics
import Data.Maybe (fromMaybe)

import Ordn.Date
import Ordn.PeriodicLog
import Ordn.Document

data Config = Config
  { dailyDir :: FilePath
  , documentsDir :: FilePath
  , templatesDir :: FilePath
  , dailyTemplateName :: FilePath
  , defaultTemplateName :: FilePath
  , periodicTemplateName :: FilePath
  , periodicLog :: [PeriodicLogEntry ChecklistItem]
  , periodicLogPath :: FilePath
  , today :: Date
  } deriving (Generic, Show)

instance Aeson.ToJSON Config where
  -- Generic

instance Aeson.FromJSON Config where
  -- Generic


defaultConfig :: Config
defaultConfig = Config
  { dailyDir = "./daily/"
  , documentsDir = "./docs/"
  , templatesDir = "./templates/"
  , dailyTemplateName = "daily.md"
  , defaultTemplateName = "default.md"
  , periodicTemplateName = "periodic.md"
  , periodicLogPath = "./.state"
  , today = Date 0 0 0
  , periodicLog = []
  }

configFilePath :: FilePath
configFilePath = "./config.json"

loadConfig :: IO Config
loadConfig = do
  configFileExists <- Dir.doesFileExist configFilePath
  config <- do
    if configFileExists
    then do
      content <- Char8.readFile configFilePath
      pure $ fromMaybe defaultConfig $ Aeson.decode content
    else pure defaultConfig

  let pLogPath = periodicLogPath config
  pLogFileExists <- Dir.doesFileExist pLogPath

  if not pLogFileExists
    then writeFile pLogPath "[]"
    else pure ()

  periodicLogFileContent <- Char8.readFile (periodicLogPath config)
  date <- Time.getZonedTime

  let
    (y, m, d) = toGregorian $ Time.localDay $ Time.zonedTimeToLocalTime date
    pLog = fromMaybe [] $ Aeson.decode periodicLogFileContent

  pure $ config { today = Date y m d, periodicLog = pLog}


templatePath :: Config -> String -> FilePath
templatePath config name = (templatesDir config) ++ name


dailyTemplatePath :: Config -> FilePath
dailyTemplatePath config = templatePath config $ dailyTemplateName config
