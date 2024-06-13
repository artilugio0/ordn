module Ordn.DocumentIO where

import qualified Ordn.Markdown as Markdown
import qualified System.Directory as Dir

import Ordn.Config
import Ordn.Document

createDocumentFromTemplate :: Environment -> String -> String -> IO()
createDocumentFromTemplate env template fileName = do
  let filePath = (getDocumentsDir env) ++ (show $ timestamp env) ++ "-" ++ fileName ++ ".md"
      table' = templateLookupTableFromEnvironment $ today env 

  shouldWrite <- confirmOverwriteIfExists filePath

  if not shouldWrite
    then pure ()
    else do
      content <- readFile $ templatePath env template

      let table = ("file_name", fileName) : table'

      case Markdown.parse content of
        Just doc ->
          writeFile filePath $ renderDocument table doc

        Nothing ->
          putStrLn "Error: could not parse document"


createDocumentFromDefaultTemplate :: Environment -> String -> IO()
createDocumentFromDefaultTemplate env =
  createDocumentFromTemplate env $ getDefaultTemplateName env


confirmOverwriteIfExists :: FilePath -> IO Bool
confirmOverwriteIfExists file = do
  doesExist <- Dir.doesFileExist file
  if doesExist
    then do
      putStrLn $ "the file " ++ file ++ " already exist. Should it be overwritten? (y/n) [n]:"
      answer <- getLine
      pure $ answer == "y"
    else pure True


getPeriodicTemplate :: Environment -> IO(Document)
getPeriodicTemplate env = do
  let filePath = periodicTemplatePath env

  fileContent <- readFile filePath
  let doc = Markdown.parse fileContent

  case doc of
    Nothing -> pure $ Document []
    Just d -> pure d
