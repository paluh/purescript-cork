module Cork.Render
  ( module Types
  , CanvasImageSourceNode
  , Context
  , Draw
  , Render
  , render
  )
  where

import Prelude

import Cork.Graphics.Canvas.CanvasElement (Dimensions) as CanvasElement
import Cork.Graphics.Canvas.Pool.Double (Pool, above, below, switch, workspace) as Double
import Cork.Render.Types (DrawCanvasImageSourceF(..))
import Cork.Render.Types (DrawCanvasImageSourceF(..)) as Types
import Data.Foldable (for_)
import Data.Hashable (hash)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Geometry.Distance (toNumber) as Distance
import Geometry.Plane (BoundingBox(..), Point(..))
import Geometry.Plane.Point (Point)
import Graphics.Canvas (CanvasElement, CanvasImageSource)
import Graphics.Canvas (Context2D, Dimensions, clearRect, drawImage, drawImageScale, getContext2D) as Canvas
import Seegee.Geometry.Distance.Units (Pixel) as Units

-- | XXX: Possibly migrate to `ImageBitmap` with
-- | hidden hash.
type CanvasImageSourceNode = { hash ∷ Int, canvasImageSource ∷ CanvasImageSource }

type Draw = DrawCanvasImageSourceF CanvasImageSourceNode
  -- | DrawPerspective (Quadrilateral Units.Pixel) CanvasImageSource
  -- | PutImageData (Point Units.Pixel) ImageData

pointHash ∷ Point Units.Pixel → Int
pointHash (Point p) = hash p

boundingBoxHash ∷ BoundingBox Units.Pixel → Int
boundingBoxHash (BoundingBox { height, width, x, y }) =
  hash (Distance.toNumber height /\ Distance.toNumber width /\ hash x /\ hash y)

drawHash ∷ Draw → Int
drawHash (DrawImage p image) =
  hash (pointHash p /\ image.hash)
drawHash (DrawImageScale bb image) =
  hash (boundingBoxHash bb /\ image.hash)

draw ∷ Canvas.Context2D → Draw → Effect Unit
draw ctx (DrawImage (Point p) image) = Canvas.drawImage ctx image.canvasImageSource p.x p.y
draw ctx (DrawImageScale (BoundingBox { height, width, x, y }) image) =
  Canvas.drawImageScale
    ctx image.canvasImageSource x y
    (Distance.toNumber width) (Distance.toNumber height)

type Context =
  { hash ∷ Maybe Int
  , pool ∷ Double.Pool
  , dimensions ∷ CanvasElement.Dimensions
  }

type Render =
  { above ∷ Array Draw
  , below ∷ Array Draw
  , workspace ∷ Array Draw
  }

render ∷ Context → Render → Effect Context
render ctx@{ dimensions, pool } { above, below, workspace } = do
  let
    dimensions' =
      { height: Distance.toNumber dimensions.physical.height
      , width: Distance.toNumber dimensions.physical.width
      }
    -- | We can cache hash on Draw level too
    currentHash = Just <<<  hash $ hash (map drawHash above) /\ hash (map drawHash below) /\ hash (map drawHash workspace)
  case ctx.hash, currentHash of
    Just prev, Just curr | prev == curr → pure ctx
    _, _ → do
      renderLayer dimensions' (Double.above pool) above
      renderLayer dimensions' (Double.below pool) below
      renderLayer dimensions' (Double.workspace pool) workspace

      pool' ← Double.switch pool
      pure { dimensions, hash: currentHash, pool: pool' }

renderLayer ∷ Canvas.Dimensions → CanvasElement → Array Draw → Effect Unit
renderLayer { height, width } canvas draws = do
  ctx ← Canvas.getContext2D canvas
  -- height ← Canvas.getCanvasHeight canvas
  -- width ← Canvas.getCanvasWidth canvas
  let
    rectangle = { x: 0.0, y: 0.0, height, width }
  Canvas.clearRect ctx rectangle
  for_ draws (draw ctx)



