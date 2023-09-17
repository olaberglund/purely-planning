module Main where

import Control.Applicative (pure)
import Control.Bind ((>>=))
import Data.Maybe (maybe)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen (AttrName(..))
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as P
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, const, discard, ($), (=<<))
import Scheduler (component)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)

main ∷ Effect Unit
main = do
  dt ← nowDateTime
  HA.runHalogenAff do
    liftEffect (window >>= document >>= setTitle "Wilma's scheduler")
    _ ← runUI component dt =<< awaitBody
    runUI faviconComponent unit =<< awaitHead

faviconComponent ∷ ∀ q i o m. H.Component q i o m
faviconComponent =
  H.mkComponent
    { initialState: const unit
    , render: const $ HH.link [ P.rel "apple-touch-icon", P.attr (AttrName "type") "image/png", P.href "images/apple-icon-180x180.png" ]
    , eval: H.mkEval H.defaultEval
    }

awaitHead ∷ Aff HTMLElement
awaitHead = do
  _ ← awaitLoad
  head ← selectElement (QuerySelector "head")
  maybe (throwError (error "Could not find head")) pure head
