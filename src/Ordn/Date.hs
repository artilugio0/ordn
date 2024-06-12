{-# LANGUAGE DeriveGeneric #-}
module Ordn.Date where

import qualified Data.Time as Time
import qualified Data.Aeson as Aeson
import GHC.Generics

data Date = Date
  { year :: Integer
  , month :: Int
  , day :: Int
  } deriving (Eq, Generic, Ord, Show)

instance Aeson.ToJSON Date where
  -- Generic

instance Aeson.FromJSON Date where
  -- Generic

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Show)

dayOfWeek :: Date -> Day
dayOfWeek (Date y m d) =
  case Time.dayOfWeek $ Time.fromGregorian y m d of
    Time.Monday -> Monday
    Time.Tuesday -> Tuesday
    Time.Wednesday -> Wednesday
    Time.Thursday -> Thursday
    Time.Friday -> Friday
    Time.Saturday -> Saturday
    Time.Sunday -> Sunday

addDays :: Int -> Date -> Date
addDays n (Date y m d) =
  let
    (newYear, newMonth, newDay) = Time.toGregorian $ Time.addDays (toInteger n) $ Time.fromGregorian y m d
  in
    Date newYear newMonth newDay

diffDays :: Date -> Date -> Integer
diffDays (Date y1 m1 d1) (Date y2 m2 d2) =
  Time.diffDays
    (Time.fromGregorian y1 m1 d1)
    (Time.fromGregorian y2 m2 d2)
