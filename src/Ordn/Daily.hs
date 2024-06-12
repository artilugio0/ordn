module Ordn.Daily
  ( createDailyFile
  )
  where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import qualified System.Directory as Dir
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8

import Ordn.Document
import Ordn.Config
import qualified Ordn.Markdown as Markdown
import qualified Ordn.DocumentIO as DocumentIO
import qualified Ordn.Periodic as Periodic


defaultDailyFile :: Document
defaultDailyFile =
  Document
  [ Heading 1 [Variable "date"]
  , Heading 2 [StringLiteral "To Do"]
  , Checklist
    [ ChecklistItem [StringLiteral "task 1"] False
    ]
  ]


createDailyFile :: Config -> IO ()
createDailyFile config = do
  let
      fileName = "daily-" ++ (showDate $ today config) ++ ".md"
      filePath = (dailyDir config) ++ fileName
      table = templateLookupTableFromConfig $ today config

  shouldWrite <- DocumentIO.confirmOverwriteIfExists filePath

  if not shouldWrite
    then pure ()
    else do
      dailyTemplateContent <- readFile $ dailyTemplatePath config
      periodicTemplate <- DocumentIO.getPeriodicTemplate config

      let todosForToday = Periodic.getTodosForToday config periodicTemplate
          dailyDoc' = fromMaybe defaultDailyFile . Markdown.parse $ dailyTemplateContent
          dailyDoc = addTodos todosForToday dailyDoc'

      let fileContent = renderDocument table dailyDoc

      writeFile filePath fileContent

      -- update log
      let
        updatedLog = Periodic.updatePeriodicLog
          (today config)
          todosForToday
          (periodicLog config)

      Char8.writeFile ((periodicLogPath config)++".tmp") (Aeson.encode updatedLog)
      Dir.removeFile $ periodicLogPath config
      Dir.copyFile ((periodicLogPath config)++".tmp") (periodicLogPath config)
      Dir.removeFile ((periodicLogPath config)++".tmp")


addTodos :: [ChecklistItem] -> Document -> Document
addTodos todos (Document doc) =
  let
    index =
      findIndex
        (\e ->
          case e of
            (Heading _ name) -> name == [StringLiteral "To Do"]
            _ -> False)
        doc

  in
    case index of
      Nothing -> Document $ doc ++ [Heading 2 [StringLiteral "To Do"], Checklist todos]
      (Just i) ->
        if length doc < i + 2
          then Document $ doc ++ [Checklist todos]
          else
            case doc !! (i+1) of
              (Checklist items) ->
                Document $ (take (i+1) doc) ++ [Checklist (items ++ todos)] ++ (drop (i+2) doc)
              _ -> Document $ (take (i+1) doc) ++ [Checklist todos] ++ (drop (i+1) doc)
