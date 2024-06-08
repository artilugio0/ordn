{-# LANGUAGE DeriveGeneric #-}
module Ordn where

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time
import Data.Time.Calendar (toGregorian)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Aeson as Aeson
import qualified System.Directory as Dir

import GHC.Generics
import qualified Data.Aeson as Aeson

-- Config

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
  } deriving Show


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


loadConfig :: IO Config
loadConfig = do
  let pLogPath = periodicLogPath defaultConfig
  pLogFileExists <- Dir.doesFileExist pLogPath

  if not pLogFileExists
    then writeFile pLogPath "[]"
    else pure ()

  periodicLogFileContent <- Char8.readFile (periodicLogPath defaultConfig)
  date <- Time.getZonedTime

  let
    (y, m, d) = toGregorian $ Time.localDay $ Time.zonedTimeToLocalTime date
    pLog = fromMaybe [] $ Aeson.decode periodicLogFileContent

  pure $ defaultConfig { today = Date y m d, periodicLog = pLog}


templatePath :: Config -> String -> FilePath
templatePath config name = (templatesDir config) ++ name


dailyTemplatePath :: Config -> FilePath
dailyTemplatePath config = templatePath config $ dailyTemplateName config


-- Date

data Date = Date
  { year :: Integer
  , month :: Int
  , day :: Int
  } deriving (Eq, Generic, Ord, Show)

instance Aeson.ToJSON Date where
  -- Generic

instance Aeson.FromJSON Date where
  -- Generic


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show)

dayOfWeek :: Date -> Day
dayOfWeek (Date y m d) =
  case Time.dayOfWeek $ Time.fromGregorian y m d of
    Time.Monday -> Monday
    Time.Tuesday -> Tuesday
    Time.Wednesday -> Wednesday
    Time.Thursday -> Thursday
    Time.Friday -> Friday
    Time.Saturday -> Saturday
    Time.Sunday -> Sunday

addDays :: Int -> Date -> Date
addDays n (Date y m d) =
  let
    (newYear, newMonth, newDay) = Time.toGregorian $ Time.addDays (toInteger n) $ Time.fromGregorian y m d
  in
    Date newYear newMonth newDay

diffDays :: Date -> Date -> Integer
diffDays (Date y1 m1 d1) (Date y2 m2 d2) =
  Time.diffDays
    (Time.fromGregorian y1 m1 d1)
    (Time.fromGregorian y2 m2 d2)


-- Periodic log

data Period
  = Daily
  | DailyExceptDays [Day]
  | EveryNDays Int (Maybe Date)
  | Weekly Day
  | Monthly Int
  deriving (Eq, Show)

isEveryNDays :: Period -> Bool
isEveryNDays (EveryNDays _ _) = True
isEveryNDays _ = False

data PeriodicItem a =
  PeriodicItem
    { period :: Period
    , piItem :: a
    } deriving (Eq, Show)

data PeriodicLogEntry a =
  PeriodicLogEntry
    { plItem :: a
    , lastUsed :: Date
    } deriving (Eq, Generic, Show)

instance Aeson.ToJSON a => Aeson.ToJSON (PeriodicLogEntry a) where
  -- Generic

instance Aeson.FromJSON a => Aeson.FromJSON (PeriodicLogEntry a) where
  -- Generic


newtype PeriodicLog = PeriodicLog [PeriodicLogEntry ChecklistItem]
  deriving (Generic, Show)

instance Aeson.FromJSON PeriodicLog where
  -- Generic

instance Aeson.ToJSON PeriodicLog where
  -- Generic

-- Document

data Document = Document [Element] deriving Show

instance Semigroup Document where
  (Document a) <> (Document b) = Document $ a ++ b

data Element
  = Heading Int StringValue
  | Paragraph StringValue
  | Checklist [ChecklistItem]
  deriving (Eq, Show)

type StringValue = [StringValueAtom]

data StringValueAtom
  = StringLiteral String
  | Variable String
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON StringValueAtom where
  -- Generic

instance Aeson.FromJSON StringValueAtom where
  -- Generic

data ChecklistItem = ChecklistItem StringValue Bool deriving (Eq, Generic, Show)

instance Aeson.ToJSON ChecklistItem where
  -- Generic

instance Aeson.FromJSON ChecklistItem where
  -- Generic

type LookupTable = [(String, String)]


-- Document render

renderDocument :: LookupTable -> Document -> String
renderDocument lookupTable (Document elements) =
  intercalate "\n\n" . map (renderElement lookupTable) $ elements


renderElement :: LookupTable -> Element -> String
renderElement lookupTable (Heading n str) =
  (take n . repeat) '#' <> " " <> (renderStringValue lookupTable str)

renderElement lookupTable (Paragraph str) =
  (renderStringValue lookupTable str)

renderElement lookupTable (Checklist items) =
  intercalate "\n" . map (renderItem lookupTable) $ items
  where
    renderItem lt (ChecklistItem str done) =
      (if done then "- [x] " else "- [ ] ") ++ (renderStringValue lt str)


renderStringValue :: LookupTable -> StringValue -> String
renderStringValue _ [] = ""
renderStringValue lookupTable (StringLiteral str:rest) = str ++ (renderStringValue lookupTable rest)
renderStringValue lookupTable (Variable var:rest) =
  (fromMaybe "" $ lookup var lookupTable) ++ (renderStringValue lookupTable rest)


replace :: String -> String -> String -> String
replace _ _ "" = ""
replace old new str =
  if old `isPrefixOf` str
    then new ++ (replace old new $ drop (length old) str)
    else head str : replace old new (tail str)


templateLookupTableFromConfig :: Config -> [(String, String)]
templateLookupTableFromConfig conf =
  [ ("date", showDate $ today conf)
  ]

showDate :: Date -> String
showDate (Date y m d) =
  intercalate "-"
  . map ((\s -> if length s == 1 then "0"++s else s) . show)
   $ [y, toInteger m, toInteger d]


-- Document DSL
heading_ :: Int -> StringValueAtom -> Document
heading_ n sa = Document [Heading n [sa]]
