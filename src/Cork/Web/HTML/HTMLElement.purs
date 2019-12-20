module Cork.Web.HTML.HTMLElement where

import Prelude

import Effect (Effect)
import Web.HTML (HTMLElement)

data Display = None | Block

foreign import setStyleProperty ∷ String → String → HTMLElement → Effect Unit

setDisplay ∷ Display → HTMLElement → Effect Unit
setDisplay display elem =
  setStyleProperty "display" (serDisplay display) elem
  where
    serDisplay None = "none"
    serDisplay Block = "block"

