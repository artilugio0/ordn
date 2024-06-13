module Main where

import qualified System.Environment as Env
import Control.Monad.Reader (runReader)

import qualified Ordn
import qualified Ordn.Config as Config

main :: IO ()
main = do
  args <- Env.getArgs
  let command = parseArgs args

  case command of
    Just CreateDailyFile ->
      createDailyFile

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
  env <- Config.loadEnvironment
  runReader Ordn.createDailyFile env


createDocumentFromDefaultTemplate :: String -> IO ()
createDocumentFromDefaultTemplate fileName = do
  env <- Config.loadEnvironment
  runReader
    (Ordn.createDocumentFromDefaultTemplate fileName)
    env


createDocumentFromTemplate :: String -> String -> IO ()
createDocumentFromTemplate fileName templateName  = do
  env <- Config.loadEnvironment
  runReader (Ordn.createDocumentFromTemplate templateName fileName) env


showUsage :: IO ()
showUsage = do
  programName <- Env.getProgName
  putStrLn $ "Usage: " ++ programName ++ " [create-daily | create-document] [ARGS]"
