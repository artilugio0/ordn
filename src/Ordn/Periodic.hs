module Ordn.Periodic where

import Text.Read (readMaybe)
import Control.Monad (join)
import Data.Char (toLower)
import Data.List (find)

import Ordn

getTodosForToday :: Config -> Document -> [ChecklistItem]
getTodosForToday conf doc =
  let
    date = today conf
    dayW = dayOfWeek $ date
    monthDay = day $ date
    periodicTodos = documentToPeriodicTodos doc
    daily = join $ map piItem $ filter (\pit -> period pit == Daily) periodicTodos
    monthly = join $ map piItem $ filter (\pit -> period pit == Monthly monthDay) periodicTodos
    weekly = join $ map piItem $ filter (\pit -> period pit == Weekly dayW) periodicTodos
    everyNdays' = filter (isEveryNDays . period) periodicTodos
    everyNdays = filterEveryNDaysForDate date (periodicLog conf) everyNdays'
  in
    daily ++ weekly ++ monthly ++ everyNdays 


documentToPeriodicTodos :: Document -> [PeriodicItem [ChecklistItem]]
documentToPeriodicTodos (Document []) = []
documentToPeriodicTodos (Document [_]) = []
documentToPeriodicTodos (Document ((Heading _ desc):(Checklist items):rest)) =
  case desc of
    [StringLiteral "Daily"] ->
      PeriodicItem { piItem = items, period = Daily } : documentToPeriodicTodos (Document rest)

    [StringLiteral "Weekly"] ->
      PeriodicItem { piItem = items, period = Weekly Monday } : documentToPeriodicTodos (Document rest)

    [StringLiteral ('W':'e':'e':'k':'l':'y':' ':dayStr)] ->
      let
        weekDay = case map toLower dayStr of
          "tuesday" -> Tuesday
          "wednesday" -> Wednesday
          "thursday" -> Thursday
          "friday" -> Friday
          "saturday" -> Saturday
          "sunday" -> Sunday
          _ -> Monday
      in
        PeriodicItem { piItem = items, period = Weekly weekDay } : documentToPeriodicTodos (Document rest)

    [StringLiteral "Monthly"] ->
      PeriodicItem { piItem = items, period = Monthly 1 } : documentToPeriodicTodos (Document rest)

    [StringLiteral ('M':'o':'n':'t':'h':'l':'y':' ':dayStr)] ->
      case readMaybe dayStr of
        Just n | n >= 1 && n <= 28 ->
          PeriodicItem { piItem = items, period = Monthly n } : documentToPeriodicTodos (Document rest)
        _ ->
          PeriodicItem { piItem = items, period = Monthly 1 } : documentToPeriodicTodos (Document rest)

    [StringLiteral ('E':'v':'e':'r':'y':' ':everyRest)] ->
      case words everyRest of
        [nStr, "days"] ->
          case readMaybe nStr of
            Just n ->
              PeriodicItem { piItem = items, period = EveryNDays n Nothing } : documentToPeriodicTodos (Document rest)
            _ ->
              PeriodicItem { piItem = items, period = EveryNDays 1 Nothing } : documentToPeriodicTodos (Document rest)

        [nStr, "days", "starting", "on", dateStr] ->
          let
            dateFields = sequenceA $ map readMaybe $ splitOn (=='-') dateStr
            date = case dateFields of
              Just [y, m, d] -> Just $ Date (toInteger y) m d
              _ -> Nothing
          in
            case readMaybe nStr of
              Just n ->
                PeriodicItem { piItem = items, period = EveryNDays n date } : documentToPeriodicTodos (Document rest)
              _ ->
                PeriodicItem { piItem = items, period = EveryNDays 1 date } : documentToPeriodicTodos (Document rest)

        _ -> documentToPeriodicTodos (Document rest)

    _ -> documentToPeriodicTodos (Document rest)

documentToPeriodicTodos (Document (_:rest)) = documentToPeriodicTodos (Document rest)


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn _ [x] = [[x]]
splitOn shouldSplit (x:y:xs) =
  if shouldSplit x
    then splitOn shouldSplit (y:xs)
    else
      if shouldSplit y
        then [x] : splitOn shouldSplit xs
        else
          case splitOn shouldSplit (y:xs) of
            (z:zs) -> (x:z):zs
            [] -> [[x]]


filterEveryNDaysForDate :: Date -> [PeriodicLogEntry ChecklistItem] -> [PeriodicItem [ChecklistItem]] -> [ChecklistItem]
filterEveryNDaysForDate date logs pits =
  map piItem
  $ filter (isPeriodicItemForDate date logs)
  $ join
  $ map unwrapPeriodicItemOfList
  $ pits


isPeriodicItemForDate :: Eq a => Date -> [PeriodicLogEntry a] -> PeriodicItem a -> Bool
isPeriodicItemForDate _ [] _ = True
isPeriodicItemForDate date (logEntry:rest) item =
  case period item of
    EveryNDays nDays _ ->
      case logEntry of
        PeriodicLogEntry it lastdate | it == piItem item ->
          addDays nDays lastdate == date -- Think about changing it to a less than comparison

        _ -> isPeriodicItemForDate date rest item

    _ -> True


unwrapPeriodicItemOfList :: PeriodicItem [a] -> [PeriodicItem a]
unwrapPeriodicItemOfList (PeriodicItem per xs) =
  map (PeriodicItem per) xs


updatePeriodicLog :: Eq a => Date -> [a] -> [PeriodicLogEntry a] -> [PeriodicLogEntry a]
updatePeriodicLog dateToday items logs =
  let
    updatedExistingEntries =
      map
        (\pit ->
          if elem (plItem pit) items
            then pit { lastUsed = dateToday }
            else pit)
        logs

    newEntries =
      map (\i -> PeriodicLogEntry { plItem = i, lastUsed = dateToday })
      $ filter
        (\it ->
          case find (\pli -> plItem pli ==  it) logs of
            Just _ -> False
            Nothing -> True)
      $ items

  in
    newEntries ++ updatedExistingEntries
