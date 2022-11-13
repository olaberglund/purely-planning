module Scheduler where

import Prelude

import Calendar as C
import Data.Array (delete, elem, take, (:))
import Data.DateTime (DateTime, time)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Form as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Type.Prelude (Proxy(..))
import Types (Workdays)
import Utils (css)

type State =
  { currentlyPicked ∷ F.Option
  , workdays ∷ Workdays
  , datetime ∷ DateTime
  }

data Action = HandlePicked F.Output | HandleDate C.Output | Reset

type Input = DateTime

type Slots =
  ( calendar ∷ ∀ q. H.Slot q C.Output Unit
  , form ∷ ∀ q. H.Slot q F.Output Unit
  )

initialState ∷ Input → State
initialState date = { currentlyPicked: Nothing, workdays: Map.empty, datetime: date }

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
    H.modify_ _ { workdays = Map.empty ∷ Workdays }

  HandleDate date → do
    H.modify_ \s → case s.currentlyPicked of
      Just p → s
        { workdays = case Map.lookup date s.workdays of
            Just shs →
              if p `elem` shs then
                Map.insert date (delete p shs) s.workdays
              else
                Map.insert date (take 2 $ p : shs) s.workdays
            Nothing → Map.insert date [ p ] s.workdays
        }
      Nothing → s
