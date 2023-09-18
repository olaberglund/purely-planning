module Form where

import Prelude

import Data.Array (cons, nubEq)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Time (Time(..))
import Effect.Aff (Aff)
import FormInputs as FI
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Shift(..), mkHours, mkShift)
import Utils (css)

type Option = Maybe Shift

type Options = Array Option

data Action = HandleForm State { | FI.Form F.FieldOutput } | ChangeOption Option

type Output = Option

type Input = Time

type State = { options ∷ Options, result ∷ Option, picked ∷ Option }

mkTime ∷ Int → Int → Maybe Time
mkTime h m = Time <$> toEnum h <*> toEnum m <*> toEnum 0 <*> toEnum 0

initialOptions ∷ Options
initialOptions =
  [ mkShift "F (06:45 - 15:30)" <$> (mkHours <$> mkTime 6 45 <*> mkTime 15 30)
  , mkShift "C (12:30 - 21:30)" <$> (mkHours <$> mkTime 12 30 <*> mkTime 21 30)
  , mkShift "C, weekend (13:30 - 21:30)" <$> (mkHours <$> mkTime 13 30 <*> mkTime 21 30)
  , mkShift "M (08:30 - 17:30)" <$> (mkHours <$> mkTime 8 30 <*> mkTime 17 30)
  , mkShift "N (20:45 - 07.15)" <$> (mkHours <$> mkTime 20 45 <*> mkTime 7 15)
  ]

initialState ∷ Input → State
initialState _ = { options: initialOptions, result: Nothing, picked: Nothing }

form ∷ ∀ q. H.Component q Input Output Aff
form = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleForm state result →
      H.modify_ _
        { options = nubEq $ cons (Just (mkShift result.label (mkHours result.from result.to))) state.options }

    ChangeOption option → do
      H.modify_ _ { picked = option }
      H.raise option

  renderOption ∷ Maybe Shift → String
  renderOption op = case op of
    Just (Shift s) → s.label
    Nothing → ""

  render state =
    HH.section [ css "shift-form" ]
      ( [ -- HH.slot (Proxy ∷ Proxy "inner") unit FI.form unit (HandleForm state)
          HH.section [ css "shifts" ]
            ( state.options <#> \option →
                HH.label_
                  [ HH.input
                      [ HP.type_ HP.InputRadio
                      , HP.name "radio"
                      , HP.checked (state.picked == option)
                      , HE.onChange (\_ → ChangeOption option)
                      , css "radio__button"
                      ]
                  , HH.text $ renderOption option
                  ]
            )

        ]
      )
