module Ordn.Daily
  ( createDailyFile
  )
  where

import Control.Monad.Reader (Reader, ask)
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import qualified System.Directory as Dir
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8

import Ordn.Document
import qualified Ordn.Config as Config
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


createDailyFile :: Reader Config.Environment (IO ())
createDailyFile = do
  env <- ask
  dailyDir <- Config.getDailyDir
  periodicLogPath <- Config.getPeriodicLogPath
  dailyTemplatePath' <- Config.dailyTemplatePath
  periodicTemplateIO <- DocumentIO.getPeriodicTemplate
  table <- Config.getLookupTable

  let
      todayDate = Config.today env
      fileName = "daily-" ++ (showDate todayDate) ++ ".md"
      filePath = dailyDir ++ fileName

  pure
    $ do
      shouldWrite <- DocumentIO.confirmOverwriteIfExists filePath

      if not shouldWrite
        then pure ()
        else do
          dailyTemplateContent <- readFile dailyTemplatePath'
          periodicTemplate <- periodicTemplateIO

          let todosForToday = Periodic.getTodosForToday env periodicTemplate
              dailyDoc' = fromMaybe defaultDailyFile . Markdown.parse $ dailyTemplateContent
              dailyDoc = addTodos todosForToday dailyDoc'

          let fileContent = renderDocument table dailyDoc

          writeFile filePath fileContent

          -- update log
          let
            updatedLog = Periodic.updatePeriodicLog
              todayDate
              todosForToday
              (Config.periodicLog env)

          Char8.writeFile (periodicLogPath ++ ".tmp") (Aeson.encode updatedLog)
          Dir.removeFile $ periodicLogPath
          Dir.copyFile (periodicLogPath ++ ".tmp") periodicLogPath
          Dir.removeFile (periodicLogPath ++ ".tmp")


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
