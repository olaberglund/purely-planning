module FormChild where

import Prelude

import Data.Array (index)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Shift, Time(..))
import UI (radioGroup)

data Action
  = Receive FormContext
  | Eval FormlessAction

type Input = { radioOptions ∷ Array (Maybe Shift) }

type Picked = Maybe Shift

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

type Form ∷ (Type → Type → Type → Type) → Row Type
type Form f =
  ( picked ∷ f Picked Void Picked
  , from ∷ f String String Time
  , to ∷ f String String Time
  , label ∷ f String String String
  )

form ∷ ∀ q. H.Component q Input { | Form F.FieldOutput } Aff
form = F.formless { liftAction: Eval } initialForm hComponent
  where

  hComponent =
    ( H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval
            { receive = Just <<< Receive
            , handleAction = handleAction
            , handleQuery = handleQuery
            }
        }
    )

  initialForm ∷ { | Form F.FieldInput }
  initialForm =
    { picked: Nothing
    , from: ""
    , to: ""
    , label: ""
    }

  handleQuery ∷ ∀ a. F.FormQuery _ _ _ _ a → H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery =
    do
      let
        splitTime ∷ String → Array String
        splitTime = split (Pattern ":")

        validateTime ∷ String → Either String Time
        validateTime s = case parseTime s of
          Just t → Right t
          _ → Left "Invalid time"

        parseTime s = do
          let components = splitTime s
          h ← toEnum =<< fromString =<< index components 0
          m ← toEnum =<< fromString =<< index components 1
          pure $ Time h m

        validation ∷ { | Form F.FieldValidation }
        validation =
          { picked: Right
          , to: validateTime
          , from: validateTime
          , label: case _ of
              "" → Left "Required"
              s → Right s
          }

      F.handleSubmitValidate F.raise F.validate validation

  render ∷ FormContext → H.ComponentHTML Action () Aff
  render { formActions, fields, actions, input } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ radioGroup
              { label: "Pick a shift"
              , options: input.radioOptions <#> \option → { option, render: show option, props: [] }
              , state: fields.picked
              , action: actions.picked
              }
          , HH.input
              [ HP.type_ HP.InputTime
              , HE.onValueInput actions.to.handleChange
              , HE.onBlur actions.to.handleBlur
              ]
          , HH.input
              [ HP.type_ HP.InputTime
              , HE.onValueInput actions.from.handleChange
              , HE.onBlur actions.from.handleBlur
              ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.label.handleChange
              , HE.onBlur actions.label.handleBlur
              ]
          ]
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]

  handleAction ∷ Action → H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    -- When we receive new form context we need to update our form state.
    Receive context →
      H.put context

    -- When a `FormlessAction` has been triggered we must raise it up to
    -- Formless for evaluation. We can do this with `F.eval`.
    Eval action →
      F.eval action
