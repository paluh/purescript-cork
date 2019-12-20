module Cork.Web.HTML.HTMLDivElement where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.HTML (HTMLDivElement, window)
import Web.HTML.HTMLDivElement (fromElement) as HTMLDivElement
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

create ∷ Effect HTMLDivElement
create = unsafePartial
  (window >>= document >>= toDocument >>> createElement "div" <#> HTMLDivElement.fromElement <#> fromJust)

