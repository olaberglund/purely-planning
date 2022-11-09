module Types where

import Prelude

import Data.Date (Date)
import Data.DateTime as Date

data Time = Time Date.Hour Date.Minute

derive instance ordTime ∷ Ord Time

derive instance eqTime ∷ Eq Time

newtype Hours = Hours { from ∷ Time, to ∷ Time }

derive instance ordHours ∷ Ord Hours
derive instance eqHours ∷ Eq Hours

mkHours ∷ Time → Time → Hours
mkHours t1 t2 = Hours { from: t1, to: t2 }

newtype Shift = Shift { label ∷ String, hours ∷ Hours }

mkShift ∷ String → Hours → Shift
mkShift s h = Shift { label: s, hours: h }

instance eqShift ∷ Eq Shift where
  eq (Shift s1) (Shift s2) = eq s1.label s2.label

instance Show Time where
  show (Time h m) = show h <> ":" <> show m

instance Show Hours where
  show (Hours { from, to }) = show from <> " - " <> show to

instance Show Shift where
  show (Shift { label }) = label

derive instance ordShift ∷ Ord Shift

data Workday = Workday Shift Date

derive instance ordWorkday ∷ Ord Workday
derive instance eqWorkday ∷ Eq Workday

instance Show Workday where
  show (Workday s d) = show s <> ", " <> show d
