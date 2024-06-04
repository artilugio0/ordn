module Ordn.Periodic where

import Ordn
import Data.List (findIndex)

getSectionChecklistItems :: String -> Document -> [ChecklistItem]
getSectionChecklistItems sectionName (Document doc) =
  let
    index =
      findIndex
        (\e ->
          case e of
            (Heading _ name) -> name == [StringLiteral sectionName]
            _ -> False)
        doc

  in
    case index of
      Nothing -> []
      (Just i) ->
        if length doc < i + 2
          then []
          else
            case doc !! (i+1) of
              (Checklist items) -> items
              _ -> []


getTodosForToday :: Config -> Document -> [ChecklistItem]
getTodosForToday _ = getSectionChecklistItems "Daily"
