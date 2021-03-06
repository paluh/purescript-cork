module Cork.Graphics.Canvas.Pool.Double where

import Prelude hiding (append)

import Cork.Graphics.Canvas.CanvasElement (Dimensions, ImageRendering) as CanvasElement
import Cork.Graphics.Canvas.Pool.Single (Pool, above, append, below, new, physicalDimensions, setImageRendering, setLogicalDimensions, setPhysicalDimensions, workspace) as Single
import Cork.Web.HTML.HTMLDivElement (create) as HTMLDivElement
import Cork.Web.HTML.HTMLElement (Display(..))
import Cork.Web.HTML.HTMLElement (setDisplay) as HTMLElement
import Effect (Effect)
import Geometry.Plane (Dimensions)
import Graphics.Canvas (CanvasElement)
import Seegee.Geometry.Distance.Units (Pixel, Screen) as Units
import Web.DOM.Node (appendChild)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLDivElement (toHTMLElement) as HTMLDivElement
import Web.HTML.HTMLElement (toNode) as HTMLElement

type Container =
  { pool ∷ Single.Pool
  , container ∷ HTMLElement
  }

newtype Pool = Pool
  { hidden ∷ Container
  , visible ∷ Container
  }

append ∷ Pool → HTMLElement → Effect Unit
append (Pool p) parentDiv = do
  void $ appendChild (HTMLElement.toNode p.hidden.container) (HTMLElement.toNode parentDiv)
  void $ appendChild (HTMLElement.toNode p.visible.container) (HTMLElement.toNode parentDiv)

new ∷ CanvasElement.Dimensions → Effect Pool
new dimensions = do
  hiddenPool ← Single.new dimensions
  visiblePool ← Single.new dimensions
  hiddenContainer ← HTMLDivElement.toHTMLElement <$> HTMLDivElement.create
  visibleContainer ← HTMLDivElement.toHTMLElement <$> HTMLDivElement.create
  Single.append hiddenPool hiddenContainer
  Single.append visiblePool visibleContainer
  switch $ Pool
    { hidden:
        { container: hiddenContainer
        , pool: hiddenPool
        }
    , visible:
        { container: visibleContainer
        , pool: visiblePool
        }
    }

switch ∷ Pool → Effect Pool
switch (Pool pool) = do
  HTMLElement.setDisplay None (pool.visible.container)
  HTMLElement.setDisplay Block (pool.hidden.container)
  pure $ Pool
    { hidden: pool.visible
    , visible: pool.hidden
    }

setImageRendering ∷ CanvasElement.ImageRendering → Pool → Effect Unit
setImageRendering imageRendering (Pool p) = do
  Single.setImageRendering imageRendering p.visible.pool
  Single.setImageRendering imageRendering p.hidden.pool

setPhysicalDimensions ∷ Dimensions Units.Pixel → Pool → Effect Unit
setPhysicalDimensions dimensions (Pool pool) = do
  Single.setPhysicalDimensions dimensions pool.visible.pool
  Single.setPhysicalDimensions dimensions pool.hidden.pool

setLogicalDimensions ∷ Dimensions Units.Screen → Pool → Effect Unit
setLogicalDimensions dimensions (Pool pool) = do
  Single.setLogicalDimensions dimensions pool.visible.pool
  Single.setLogicalDimensions dimensions pool.hidden.pool

setDimensions ∷ CanvasElement.Dimensions → Pool → Effect Unit
setDimensions { logical, physical } pool = do
  setLogicalDimensions logical pool
  setPhysicalDimensions physical pool

physicalDimensions ∷ Pool → Effect (Dimensions Units.Pixel)
physicalDimensions (Pool pool) = Single.physicalDimensions pool.visible.pool

above ∷ Pool → CanvasElement
above (Pool p) = Single.above p.hidden.pool

below ∷ Pool → CanvasElement
below (Pool p) = Single.below p.hidden.pool

workspace ∷ Pool → CanvasElement
workspace (Pool p) = Single.workspace p.hidden.pool

