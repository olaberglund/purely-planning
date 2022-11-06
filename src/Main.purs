module Main where

import Effect (Effect)
import Effect.Now (nowDateTime)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind)
import Scheduler (component)

main ∷ Effect Unit
main = do
  dt ← nowDateTime
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI component dt body
