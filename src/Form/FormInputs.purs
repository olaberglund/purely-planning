module FormInputs where

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
import Types (Time(..))
import Utils (css)

data Action
  = Receive FormContext
  | Eval FormlessAction

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

type Form ∷ (Type → Type → Type → Type) → Row Type
type Form f =
  ( from ∷ f String String Time
  , to ∷ f String String Time
  , label ∷ f String String String
  )

form ∷ ∀ q. H.Component q Unit { | Form F.FieldOutput } Aff
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
    { from: ""
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
          { to: validateTime
          , from: validateTime
          , label: case _ of
              "" → Left "Required"
              s → Right s
          }

      F.handleSubmitValidate F.raise F.validate validation

  render ∷ FormContext → H.ComponentHTML Action () Aff
  render { formActions, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit, css "form" ]
      [ HH.fieldset [ css "fields" ]
          [ HH.label [ HP.for "from" ] [ HH.text "From" ]
          , HH.input
              [ HP.type_ HP.InputTime
              , HE.onValueInput actions.from.handleChange
              , HE.onBlur actions.from.handleBlur
              , HP.name "from"
              ]
          , HH.label [ HP.for "to" ] [ HH.text "To" ]
          , HH.input
              [ HP.type_ HP.InputTime
              , HE.onValueInput actions.to.handleChange
              , HE.onBlur actions.to.handleBlur
              , HP.name "to"
              ]
          , HH.label [ HP.for "label" ] [ HH.text "Label" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.label.handleChange
              , HE.onBlur actions.label.handleBlur
              , HP.name "label"
              ]
          ]
      , HH.button
          [ css "button", HP.type_ HP.ButtonSubmit ]
          [ HH.text "Add" ]
      ]

  handleAction ∷ Action → H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    -- When we receive new form context we need to update our form state.
    Receive context → do
      H.put context

    -- When a `FormlessAction` has been triggered we must raise it up to
    -- Formless for evaluation. We can do this with `F.eval`.
    Eval action →
      F.eval action
