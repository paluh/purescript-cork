module Cork.Web.HTML.HTMLDocument where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element)
import Web.DOM.Document (toNonElementParentNode) as Document
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, HTMLElement)
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (fromElement) as HTMLElement

getHTMLElementById :: String → HTMLDocument -> Effect (Maybe HTMLElement)
getHTMLElementById id document = map unsafeToHTMLElement <$>
  getElementById id (HTMLDocument.toDocument >>> Document.toNonElementParentNode $ document)
  where
    unsafeToHTMLElement ∷ Element → HTMLElement
    unsafeToHTMLElement = unsafePartial $ fromJust <<< HTMLElement.fromElement
