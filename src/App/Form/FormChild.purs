module FormChild where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Types (Shift)
import UI (radioGroup)

data Action
  = Receive FormContext
  | Eval FormlessAction

type Input = { radioOptions ∷ Array (Maybe Shift) }

type Picked = Maybe Shift

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

type Form ∷ (Type → Type → Type → Type) → Row Type
type Form f = (picked ∷ f Picked Void Picked)

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
  initialForm = { picked: Nothing }

  handleQuery ∷ ∀ a. F.FormQuery _ _ _ _ a → H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = F.handleSubmitValidate F.raise F.validate
    { picked: Right }

  render ∷ FormContext → H.ComponentHTML Action () Aff
  render { formActions, fields, actions, input } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ radioGroup
              { label: "Pick One"
              , options: input.radioOptions <#> \option → { option, render: show option, props: [] }
              , state: fields.picked
              , action: actions.picked
              }
          ]
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
