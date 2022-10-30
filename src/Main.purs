module Main where

import Data.DateTime (time)
import Effect (Effect)
import Effect.Now (nowDateTime)
import FormParent (formParent)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind)

main ∷ Effect Unit
main = do
  dt ← nowDateTime
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI formParent (time dt) body
