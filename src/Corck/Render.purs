module Corck.Render where

import Prelude

import Corck.Graphics.Canvas.CanvasElement (Dimensions) as CanvasElement
import Corck.Graphics.Canvas.Pool.Double (Pool, above, below, switch, workspace) as Double
import Data.Foldable (for_)
import Data.List (List)
import Effect (Effect)
import Geometry.Distance (toNumber) as Distance
import Geometry.Plane (BoundingBox(..), Point(..))
import Geometry.Plane.Point (Point)
import Graphics.Canvas (CanvasElement, CanvasImageSource)
import Graphics.Canvas (Context2D, Dimensions, clearRect, drawImage, drawImageScale, getContext2D) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units

data Draw
  = DrawImage (Point Units.Pixel) CanvasImageSource
  | DrawImageScale (BoundingBox Units.Pixel) CanvasImageSource
  -- | DrawPerspective (Quadrilateral Units.Pixel) CanvasImageSource
  -- | PutImageData (Point Units.Pixel) ImageData

draw ∷ Canvas.Context2D → Draw → Effect Unit
draw ctx (DrawImage (Point p) image) = Canvas.drawImage ctx image p.x p.y
draw ctx (DrawImageScale (BoundingBox { height, width, x, y }) image) =
  Canvas.drawImageScale ctx image x y (Distance.toNumber width) (Distance.toNumber height)

type Context =
  { pool ∷ Double.Pool
  , dimensions ∷ CanvasElement.Dimensions
  }

type Render =
  { above ∷ List Draw
  , below ∷ List Draw
  , workspace ∷ List Draw
  }

render ∷ Context → Render → Effect Context
render { dimensions, pool } { above, below, workspace } = do
  let
    dimensions' =
      { height: Distance.toNumber dimensions.physical.height
      , width: Distance.toNumber dimensions.physical.width
      }

  renderLayer dimensions' (Double.above pool) above
  renderLayer dimensions' (Double.below pool) below
  renderLayer dimensions' (Double.workspace pool) workspace

  pool' ← Double.switch pool
  pure { dimensions, pool: pool' }

renderLayer ∷ Canvas.Dimensions → CanvasElement → List Draw → Effect Unit
renderLayer { height, width } canvas draws = do
  ctx ← Canvas.getContext2D canvas
  -- height ← Canvas.getCanvasHeight canvas
  -- width ← Canvas.getCanvasWidth canvas
  let
    rectangle = { x: 0.0, y: 0.0, height, width }
  Canvas.clearRect ctx rectangle
  for_ draws (draw ctx)



