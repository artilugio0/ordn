module Ordn.DocumentIO where

import qualified Ordn.Markdown as Markdown
import qualified System.Directory as Dir

import Control.Monad.Reader (Reader, ask)

import Ordn.Config
import Ordn.Document

createDocumentFromTemplate :: String -> String -> Reader Environment (IO ())
createDocumentFromTemplate template fileName = do
  env <- ask
  docsDir <- getDocumentsDir
  templatePath' <- templatePath template

  let timestampStr = show $ timestamp env
      todayDate = today env

  let filePath = docsDir ++ timestampStr ++ "-" ++ fileName ++ ".md"
      table' = templateLookupTableFromEnvironment todayDate

  pure
    $ do
      shouldWrite <- confirmOverwriteIfExists filePath

      if not shouldWrite
        then pure ()
        else do
          content <- readFile templatePath'

          let table = ("file_name", fileName) : table'

          case Markdown.parse content of
            Just doc ->
              writeFile filePath $ renderDocument table doc

            Nothing ->
              putStrLn "Error: could not parse document"


createDocumentFromDefaultTemplate :: String -> Reader Environment (IO ())
createDocumentFromDefaultTemplate name = do
  defaultTemplateName <- getDefaultTemplateName
  createDocumentFromTemplate defaultTemplateName name


confirmOverwriteIfExists :: FilePath -> IO Bool
confirmOverwriteIfExists file = do
  doesExist <- Dir.doesFileExist file
  if doesExist
    then do
      putStrLn $ "the file " ++ file ++ " already exist. Should it be overwritten? (y/n) [n]:"
      answer <- getLine
      pure $ answer == "y"
    else pure True


getPeriodicTemplate :: Reader Environment (IO Document)
getPeriodicTemplate = do
  filePath <- periodicTemplatePath

  let
    doc = do
      fileContent <- readFile filePath
      let doc = Markdown.parse fileContent

      case doc of
        Nothing -> pure $ Document []
        Just d -> pure d

  pure doc
