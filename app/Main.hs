module Main where

import qualified System.Environment as Env

import qualified Ordn
import qualified Ordn.Daily
import qualified Ordn.DocumentIO

main :: IO ()
main = do
  args <- Env.getArgs
  let command = parseArgs args

  case command of
    Just CreateDailyFile -> createDailyFile

    Just (CreateDocumentFromTemplate t n) ->
      createDocumentFromTemplate t n

    Just (CreateDocumentFromDefaultTemplate n) ->
      createDocumentFromDefaultTemplate n

    Nothing -> showUsage

data Command
  = CreateDailyFile
  | CreateDocumentFromDefaultTemplate String
  | CreateDocumentFromTemplate String String


parseArgs :: [String] -> Maybe Command
parseArgs [] = Just defaultCommand
parseArgs ["create-daily"] = Just CreateDailyFile
parseArgs ["create-document", name] =
  Just $ CreateDocumentFromDefaultTemplate name
parseArgs ["create-document", name, template] =
  Just $ CreateDocumentFromTemplate name template
parseArgs _ = Nothing


defaultCommand :: Command
defaultCommand = CreateDailyFile


createDailyFile :: IO ()
createDailyFile = do
  config <- Ordn.loadConfig
  Ordn.Daily.createDailyFile config


createDocumentFromDefaultTemplate :: String -> IO ()
createDocumentFromDefaultTemplate fileName = do
  config <- Ordn.loadConfig
  Ordn.DocumentIO.createDocumentFromDefaultTemplate
    config
    fileName


createDocumentFromTemplate :: String -> String -> IO ()
createDocumentFromTemplate fileName templateName  = do
  config <- Ordn.loadConfig
  Ordn.DocumentIO.createDocumentFromTemplate
    config
    templateName
    fileName


showUsage :: IO ()
showUsage = do
  programName <- Env.getProgName
  putStrLn $ "Usage: " ++ programName ++ " [create-daily | create-document] [ARGS]"
