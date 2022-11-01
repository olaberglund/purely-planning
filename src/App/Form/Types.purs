module Types where

import Prelude

import Data.DateTime as Date

data Time = Time Date.Hour Date.Minute

derive instance eqTime ∷ Eq Time

type Hours = { from ∷ Time, to ∷ Time }

hours ∷ Time → Time → Hours
hours t1 t2 = { from: t1, to: t2 }

newtype Shift = Shift { label ∷ String, hours ∷ Hours }

shift ∷ String → Hours → Shift
shift s h = Shift { label: s, hours: h }

derive newtype instance eqShift ∷ Eq Shift

instance Show Time where
  show (Time h m) = show h <> ":" <> show m

instance Show Shift where
  show (Shift { label }) = show label
