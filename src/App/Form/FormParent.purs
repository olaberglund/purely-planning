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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Prelude (Proxy(..))
import Types (Shift(..), hours, shift)

type Options = Array (Maybe Shift)

data Action = HandleForm State { | FC.Form F.FieldOutput } | ChangeOption (Maybe Shift)

type Input = Date.Time

type State = { options ∷ Options, result ∷ Maybe Shift, picked ∷ Maybe Shift }

initialState ∷ Input → State
initialState _ = { options: [], result: Nothing, picked: Nothing }

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
        { options = nubEq $ cons (Just (shift result.label (hours result.from result.to))) state.options }

    ChangeOption option →
      H.modify_ _
        { picked = option }

  renderOption ∷ Maybe Shift → String
  renderOption op = case op of
    Just (Shift s) → s.label
    Nothing → ""

  render state =
    HH.article_
      ( [ HH.h1_ [ HH.text "Form" ]
        , HH.p_ [ HH.text "this is a form with dynamic options" ]
        , case state.result of
            Just s → HH.p_ [ HH.text $ "You've picked: " <> show s ]
            Nothing → HH.p_ [ HH.text "Please create a shift!" ]
        , HH.slot (Proxy ∷ Proxy "inner") unit FC.form unit (HandleForm state)
        , HH.div_ $ map (HH.text <<< show) state.options
        , HH.text $ "You have picked: " <> (show state.picked)
        , HH.form_
            [ HH.label_ [ HH.text "Pick a shift" ]
            , HH.fieldset_
                ( state.options <#> \option →
                    HH.label_
                      [ HH.input
                          [ HP.type_ HP.InputRadio
                          , HP.name "radio"
                          , HP.checked (state.picked == option)
                          , HE.onChange (\_ → ChangeOption option)
                          ]
                      , HH.text $ renderOption option
                      ]
                )
            ]
        ]
      )
