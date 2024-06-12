{-# LANGUAGE DeriveGeneric #-}
module Ordn.PeriodicLog where

import GHC.Generics
import qualified Data.Aeson as Aeson

import Ordn.Date
import Ordn.Document

data Period
  = Daily
  | DailyExceptDays [Day]
  | EveryNDays Int (Maybe Date)
  | Weekly Day
  | Monthly Int
  deriving (Eq, Show)

isEveryNDays :: Period -> Bool
isEveryNDays (EveryNDays _ _) = True
isEveryNDays _ = False

data PeriodicItem a =
  PeriodicItem
    { period :: Period
    , piItem :: a
    } deriving (Eq, Show)

data PeriodicLogEntry a =
  PeriodicLogEntry
    { plItem :: a
    , lastUsed :: Date
    } deriving (Eq, Generic, Show)

instance Aeson.ToJSON a => Aeson.ToJSON (PeriodicLogEntry a) where
  -- Generic

instance Aeson.FromJSON a => Aeson.FromJSON (PeriodicLogEntry a) where
  -- Generic


newtype PeriodicLog = PeriodicLog [PeriodicLogEntry ChecklistItem]
  deriving (Generic, Show)

instance Aeson.FromJSON PeriodicLog where
  -- Generic

instance Aeson.ToJSON PeriodicLog where
  -- Generic
