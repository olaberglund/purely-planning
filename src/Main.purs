module Main where

import Prelude (Unit, bind)

import DayPicker
import Effect (Effect)
import Effect.Now (nowDate)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main ∷ Effect Unit
main = do
  today ← nowDate
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI component today body
