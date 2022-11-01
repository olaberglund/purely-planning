module FormParent where

import Prelude

import Data.Array (cons, nubEq)
import Data.DateTime as Date
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import FormChild as FC
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Types (Shift, hours, shift)

type Options = Array (Maybe Shift)

data Action = HandleForm State { | FC.Form F.FieldOutput }

type Input = Date.Time

type State = { options ∷ Options, result ∷ Maybe Shift }

initialState ∷ Input → State
initialState _ = { options: [], result: Nothing }

formParent ∷ ∀ q o. H.Component q Input o Aff
formParent = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleForm state result →
      H.modify_ _
        { result = Just (shift result.label (hours result.from result.to))
        , options = nubEq $ cons (Just (shift result.label (hours result.from result.to))) state.options
        }

  render state =
    HH.article_
      ( [ HH.h1_ [ HH.text "Form" ]
        , HH.p_ [ HH.text "this is a form with dynamic options" ]
        , case state.result of
            Just s → HH.p_ [ HH.text $ "You've picked: " <> show s ]
            Nothing → HH.p_ [ HH.text "Please create a shift!" ]
        ] <> [ HH.slot (Proxy ∷ Proxy "inner") unit FC.form { radioOptions: state.options } (HandleForm state) ]
      )
