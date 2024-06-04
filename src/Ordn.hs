module Ordn where

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

-- Config

data Config = Config
  { dailyDir :: FilePath
  , documentsDir :: FilePath
  , templatesDir :: FilePath
  , dailyTemplateName :: FilePath
  , defaultTemplateName :: FilePath
  , periodicTemplateName :: FilePath
  , today :: Date
  } deriving Show


data Date = Date
  { year :: Integer
  , month :: Int
  , day :: Int
  } deriving Show


defaultConfig :: Config
defaultConfig = Config
  { dailyDir = "./daily/"
  , documentsDir = "./docs/"
  , templatesDir = "./templates/"
  , dailyTemplateName = "daily.md"
  , defaultTemplateName = "default.md"
  , periodicTemplateName = "periodic.md"
  , today = Date 0 0 0
  }


loadConfig :: IO Config
loadConfig = do
  date <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay date
  pure $ defaultConfig { today = Date y m d}


templatePath :: Config -> String -> FilePath
templatePath config name = (templatesDir config) ++ name


dailyTemplatePath :: Config -> FilePath
dailyTemplatePath config = templatePath config $ dailyTemplateName config

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
  deriving (Eq, Show)

data ChecklistItem = ChecklistItem StringValue Bool deriving (Eq, Show)

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
