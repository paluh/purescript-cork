module Frontend.Utils.Canvas.Pool where

-- | XXX: (Re)consider:
-- | * Migration to static three layer pool:
-- |  - below
-- |  - dynamic
-- |  - above
-- | * Caching rendering context for every stack layer + patterns ?

import Prelude

import Cork.Graphics.Canvas.CanvasElement (Dimensions)
import Cork.Graphics.Canvas.CanvasElement (new, setLogicalDimensions, setPhysicalDimensions, toHTMLCanvasElement) as CanvasElement
import Effect (Effect)
import Geometry.Plane.BoundingBox.Dimensions (Dimensions) as Dimensions
import Graphics.Canvas (CanvasElement)
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Web.DOM.Element (setAttribute)
import Web.DOM.Node (appendChild)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLCanvasElement (toHTMLElement, toNode) as HTMLCanvasElement
import Web.HTML.HTMLElement (toElement, toNode) as HTMLElement

newtype Pool = Pool
  { above ∷ CanvasElement
  , below ∷ CanvasElement
  , workspace ∷ CanvasElement
  }

new ∷ Dimensions → Effect Pool
new dimensions = do
  let
    setStyle zIndex canvas =
      setAttribute
        "style"
        ("position: absolute; z-index:" <> show zIndex)
        (HTMLElement.toElement $ HTMLCanvasElement.toHTMLElement $ CanvasElement.toHTMLCanvasElement canvas)
  b ← CanvasElement.new dimensions
  setStyle 1 b
  w ← CanvasElement.new dimensions
  setStyle 2 w
  a ← CanvasElement.new dimensions
  setStyle 3 a
  pure $ Pool { above: a, below: b, workspace: w }

setPhysicalDimensions ∷ Dimensions.Dimensions Units.Pixel → Pool → Effect Unit
setPhysicalDimensions dimensions (Pool p) = do
  CanvasElement.setPhysicalDimensions dimensions p.above
  CanvasElement.setPhysicalDimensions dimensions p.below
  CanvasElement.setPhysicalDimensions dimensions p.workspace

setLogicalDimensions ∷ Dimensions.Dimensions Units.Screen → Pool → Effect Unit
setLogicalDimensions dimensions (Pool p) = do
  CanvasElement.setLogicalDimensions dimensions p.above
  CanvasElement.setLogicalDimensions dimensions p.below
  CanvasElement.setLogicalDimensions dimensions p.workspace

append ∷ Pool → HTMLElement → Effect Unit
append (Pool p) parentDiv = do
  let
    parentNode = HTMLElement.toNode parentDiv
    canvasNode c = HTMLCanvasElement.toNode $ CanvasElement.toHTMLCanvasElement c
  void $ appendChild
    (canvasNode p.below)
    parentNode
  void $ appendChild
    (canvasNode p.workspace)
    parentNode
  void $ appendChild
    (canvasNode p.above)
    parentNode

above ∷ Pool → CanvasElement
above (Pool p) = p.above

below ∷ Pool → CanvasElement
below (Pool p) = p.below

workspace ∷ Pool → CanvasElement
workspace (Pool p) = p.workspace

