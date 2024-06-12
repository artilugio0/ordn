module Ordn.DocumentIO where

import qualified Ordn.Markdown as Markdown
import qualified System.Directory as Dir

import Ordn.Config
import Ordn.Document

createDocumentFromTemplate :: Config -> String -> String -> IO()
createDocumentFromTemplate config template fileName = do
  let filePath = (documentsDir config) ++ fileName ++ ".md"
      table' = templateLookupTableFromConfig $ today config

  shouldWrite <- confirmOverwriteIfExists filePath

  if not shouldWrite
    then pure ()
    else do
      content <- readFile $ templatePath config template

      let table = ("file_name", fileName) : table'

      case Markdown.parse content of
        Just doc ->
          writeFile filePath $ renderDocument table doc

        Nothing ->
          putStrLn "Error: could not parse document"


createDocumentFromDefaultTemplate :: Config -> String -> IO()
createDocumentFromDefaultTemplate config =
  createDocumentFromTemplate config (defaultTemplateName config)


confirmOverwriteIfExists :: FilePath -> IO Bool
confirmOverwriteIfExists file = do
  doesExist <- Dir.doesFileExist file
  if doesExist
    then do
      putStrLn $ "the file " ++ file ++ " already exist. Should it be overwritten? (y/n) [n]:"
      answer <- getLine
      pure $ answer == "y"
    else pure True


getPeriodicTemplate :: Config -> IO(Document)
getPeriodicTemplate config = do
  let filePath = templatePath config $ periodicTemplateName config

  fileContent <- readFile filePath
  let doc = Markdown.parse fileContent

  case doc of
    Nothing -> pure $ Document []
    Just d -> pure d
