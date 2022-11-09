module Scheduler where

import Prelude

import Calendar as C
import Data.DateTime (DateTime, time)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Aff)
import Form as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Type.Prelude (Proxy(..))
import Types (Workday(..))
import Utils (css)

type State =
  { currentlyPicked ∷ F.Option
  , workdays ∷ Set.Set Workday
  , datetime ∷ DateTime
  }

data Action = HandlePicked F.Output | HandleDate C.Output | Reset

type Input = DateTime

type Slots =
  ( calendar ∷ ∀ q. H.Slot q C.Output Unit
  , form ∷ ∀ q. H.Slot q F.Output Unit
  )

initialState ∷ Input → State
initialState date = { currentlyPicked: Nothing, workdays: Set.empty, datetime: date }

component ∷ ∀ q o. H.Component q Input o Aff
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render ∷ State → H.ComponentHTML Action Slots Aff
render state =
  HH.div [ css "app" ]
    [ HH.div [ css "calendar-container" ]
        [ HH.slot (Proxy ∷ Proxy "calendar") unit C.calendar { now: state.datetime, workdays: state.workdays } HandleDate
        , HH.button [ onClick (\_ → Reset), css "button" ] [ HH.text "Reset" ]
        ]
    , HH.slot (Proxy ∷ Proxy "form") unit F.form (time state.datetime) HandlePicked
    ]

handleAction ∷ ∀ cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  HandlePicked option → do
    H.modify_ _ { currentlyPicked = option }

  Reset → do
    H.modify_ _ { workdays = Set.empty ∷ Set.Set Workday }

  HandleDate date → do
    H.modify_ \s → case s.currentlyPicked of
      Nothing → s
      Just p → s { workdays = Set.toggle (Workday p date) s.workdays }
