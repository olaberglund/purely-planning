module Types where

import Prelude

import Data.DateTime as Date

data Time = Time Date.Hour Date.Minute

derive instance eqTime ∷ Eq Time

type Hours = { from ∷ Time, to ∷ Time }

newtype Shift = Shift { label ∷ String, hours ∷ Hours }

derive newtype instance eqShift ∷ Eq Shift

instance Show Time where
  show (Time h m) = show h <> ":" <> show m

instance Show Shift where
  show (Shift { label }) = show label
