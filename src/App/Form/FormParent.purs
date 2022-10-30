module FormParent where

import Data.Either
import Data.Maybe
import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (replicate)
import Data.Const (Const)
import Data.DateTime as Date
import Data.Int as Int
import Effect.Aff (Aff)
import FormChild as FC (Form, form, Input)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

data Action = HandleForm { | FC.Form F.FieldInput } | ChangeOptions FC.Input

-- old mock:
-- where
-- shiftHours ∷ Hours
-- shiftHours = { from: (Time (Date.hour input) (Date.minute input)), to: (Time (Date.hour input) (Date.minute input)) }

formParent ∷ ∀ q i o. H.Component q i o Aff
formParent = H.mkComponent
  { initialState: \_ → { options: [], result: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleForm result →
      H.modify_ _ { result = Just result }

    ChangeOptions options →
      H.modify_ _ { options = options }

  render state =
    HH.article_
      [ HH.h1_ [ HH.text "Form" ]
      , HH.p_ [ HH.text "this is a form with dynamic options" ]
      , HH.slot (Proxy ∷ Proxy "inner") unit form { radioOptions: state.options } HandleForm
      , HH.button
          [ HE.onClick \_ → ChangeOptions (state.options <> []) ]
          [ HH.text "Add 'Three' to options" ]
      ]

-----
