module FormParent where

import Prelude

import Data.Array (head, replicate, tail)
import Data.DateTime as Date
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import FormChild as FC
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Types (Hours, Shift(..), Time(..))

type Options = Array Shift

data Action = HandleForm { | FC.Form F.FieldInput } | ChangeOptions Options | RemoveFirst Options

type Input = Date.Time

type State = { options ∷ Options, result ∷ (Maybe Shift) }

initialState ∷ Input → State
initialState input = { options: replicate 2 (Shift { label: "C", hours: shiftHours }), result: Nothing }
  where
  shiftHours ∷ Hours
  shiftHours = { from: (Time (Date.hour input) (Date.minute input)), to: (Time (Date.hour input) (Date.minute input)) }

formParent ∷ ∀ q o. H.Component q Input o Aff
formParent = H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleForm result →
      H.modify_ _ { result = result.picked }

    ChangeOptions options →
      H.modify_ _ { options = options }

    RemoveFirst options →
      H.modify_ _ { options = fromMaybe [] (tail options) }

  render state =
    HH.article_
      [ HH.h1_ [ HH.text "Form" ]
      , HH.p_ [ HH.text "this is a form with dynamic options" ]
      , HH.slot (Proxy ∷ Proxy "inner") unit FC.form { radioOptions: map Just state.options } HandleForm
      , HH.button
          [ HE.onClick \_ → ChangeOptions $ state.options <> fromMaybe [] (tail state.options) ]
          [ HH.text "Add 'Three' to options" ]
      , HH.button
          [ HE.onClick \_ → RemoveFirst $ state.options ]
          [ HH.text "Remove first option" ]
      ]

-----
