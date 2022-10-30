module UI where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type RadioGroup action input output =
  { label ∷ String
  , state ∷ F.FieldState input Void output
  , action ∷ F.FieldAction action input Void output
  , options ∷
      Array
        { option ∷ input
        , render ∷ String
        , props ∷ Array (HP.IProp HTMLinput action)
        }
  }

radioGroup
  ∷ ∀ input output action slots m
  . Eq input
  ⇒ RadioGroup action input output
  → H.ComponentHTML action slots m
radioGroup { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } →
        HH.label_
          [ HH.input $ flip append props
              [ HP.type_ HP.InputRadio
              , HP.name action.key
              , HP.checked (state.value == option)
              , HE.onChange (\_ → action.handleChange option)
              , HE.onBlur action.handleBlur
              ]
          , HH.text render
          ]
    ]
