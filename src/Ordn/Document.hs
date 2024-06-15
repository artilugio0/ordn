{-# LANGUAGE DeriveGeneric #-}
module Ordn.Document where

import qualified Data.Aeson as Aeson
import GHC.Generics
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)

import Ordn.Date

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


showDate :: Date -> String
showDate (Date y m d) =
  intercalate "-"
  . map ((\s -> if length s == 1 then "0"++s else s) . show)
   $ [y, toInteger m, toInteger d]


-- Document DSL
heading_ :: Int -> StringValueAtom -> Document
heading_ n sa = Document [Heading n [sa]]
