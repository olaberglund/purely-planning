module Main where

import App.Form.FormParent (formParent)
import Data.DateTime (time)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Now (nowDate, nowDateTime)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, (=<<))

main ∷ Effect Unit
main = do
  dt ← nowDateTime
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI formParent (time dt) body
