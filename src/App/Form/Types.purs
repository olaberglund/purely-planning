module Types where

import Prelude

import Data.DateTime as Date

data Time = Time Date.Hour Date.Minute

derive instance eqTime ∷ Eq Time

newtype Hours = Hours { from ∷ Time, to ∷ Time }

hours ∷ Time → Time → Hours
hours t1 t2 = Hours { from: t1, to: t2 }

newtype Shift = Shift { label ∷ String, hours ∷ Hours }

shift ∷ String → Hours → Shift
shift s h = Shift { label: s, hours: h }

instance eqShift ∷ Eq Shift where
  eq (Shift s1) (Shift s2) = eq s1.label s2.label

instance Show Time where
  show (Time h m) = show h <> ":" <> show m

instance Show Hours where
  show (Hours { from, to }) = show from <> "to" <> show to

instance Show Shift where
  show (Shift { label, hours }) = show label <> show hours
