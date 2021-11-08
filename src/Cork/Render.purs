module Cork.Render
  ( module Types
  , CanvasImageSourceNode
  , Context
  , Draw
  , ImageDataNode
  , Render
  , render
  )
  where

import Prelude

import Cork.Graphics.Canvas (TilesNumber(..))
import Cork.Graphics.Canvas (drawImage, drawImagePerspective, drawImageScale) as Cork.Graphics.Canvas
import Cork.Graphics.Canvas.Context2D (resetTransform)
import Cork.Graphics.Canvas.ImageBitmap.Types (ImageBitmap)
import Cork.Graphics.Canvas.Pool.Double (Pool, above, below, physicalDimensions, switch, workspace) as Double
import Cork.Hashable (boundingBox, drawingStyle, point, quadrilateral) as Cork.Hashable
import Cork.Render.Types (DrawCanvasImageSourceF(..), DrawF(..), DrawImageDataF(..), DrawingStyle)
import Cork.Render.Types (defaultStyle, DrawingStyle, DrawCanvasImageSourceF(..)) as Types
import Cork.Render.Zoom (Zoom(..))
import Data.Bifoldable (bifoldMap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Hashable (hash)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un) as Newtype
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Geometry.Plane (Point(..), Vector(..))
import Geometry.Plane.BoundingBox.Dimensions (toNumbers) as Dimensions
import Graphics.Canvas (CanvasElement, ImageData)
import Graphics.Canvas (Composite(..), Context2D, Dimensions, clearRect, getContext2D, putImageData, restore, save, scale, setGlobalAlpha, setGlobalCompositeOperation, translate) as Canvas

-- | XXX: Possibly migrate to `ImageBitmap` with
-- | hidden hash.
type CanvasImageSourceNode = { hash ∷ Int, canvasImageSource ∷ ImageBitmap }
type ImageDataNode = { hash ∷ Int, imageData ∷ ImageData }

-- | XXX: Rename to `Drawing`
type Draw = DrawF ImageDataNode CanvasImageSourceNode
  -- | DrawPerspective (Quadrilateral Units.Pixel) CanvasImageSource
  -- | PutImageData (Point Units.Pixel) ImageData

drawHash ∷ Draw → Int
drawHash (DrawF e) = Newtype.un Additive <<< bifoldMap (wrap <<< putImageDataHash) (wrap <<< drawImageHash) $ e
  where
    drawImageHash (DrawImage p style image) =
      hash (Cork.Hashable.point p /\ Cork.Hashable.drawingStyle style /\ image.hash)
    drawImageHash (DrawImageScale bb style image) =
      hash (Cork.Hashable.boundingBox bb /\ Cork.Hashable.drawingStyle style /\image.hash)
    drawImageHash (DrawImagePerspective quad (TilesNumber tilesNumber) style image) =
      hash (Cork.Hashable.quadrilateral quad /\ tilesNumber /\ Cork.Hashable.drawingStyle style /\image.hash)

    putImageDataHash (PutImageData p imageData) =
      hash (Cork.Hashable.point p /\ imageData.hash)

styleDraw ∷ Canvas.Context2D → DrawingStyle → Effect Unit → Effect Unit
styleDraw _ { compositeOperation: Canvas.SourceOver, alpha: 1.0 } d = d
styleDraw ctx style d = do
  Canvas.save ctx
  Canvas.setGlobalAlpha ctx style.alpha
  Canvas.setGlobalCompositeOperation ctx style.compositeOperation
  d
  Canvas.restore ctx

draw ∷ Canvas.Context2D → Draw → Effect Unit
draw ctx (DrawF (Left (PutImageData (Point p) image))) =
  Canvas.putImageData ctx image.imageData p.x p.y
draw ctx (DrawF (Right (DrawImage p style image))) = styleDraw ctx style $
  Cork.Graphics.Canvas.drawImage ctx image.canvasImageSource p
draw ctx (DrawF (Right (DrawImageScale bb style image))) = styleDraw ctx style $
  Cork.Graphics.Canvas.drawImageScale
    ctx image.canvasImageSource bb
draw ctx (DrawF (Right (DrawImagePerspective quad tilesNumber style image))) = styleDraw ctx style $
  Cork.Graphics.Canvas.drawImagePerspective ctx image.canvasImageSource quad tilesNumber

type Context =
  { hash ∷ Maybe Int
  , pool ∷ Double.Pool
  , zoom ∷ Zoom
  }

type Render =
  { above ∷ Array Draw
  , below ∷ Array Draw
  , workspace ∷ Array Draw
  }

render ∷ Context → Render → Effect Context
render ctx@{ pool } r@{ above, below, workspace } = do
  physicalDimensions ← Dimensions.toNumbers <$> Double.physicalDimensions pool
  -- traceM "Cork.Render called"
  -- | We are not updating hash anymore.
  -- | Hash should be provided by models and used in `nextStatus` method
  { hash: ctx.hash, pool: _, zoom: ctx.zoom } <$> renderLayers physicalDimensions ctx.zoom pool r
    -- | XXX: This should be done on the layer level!
    -- CompareHashes → case ctx.hash, currentHash of
    --   Just prev, Just curr | prev == curr → do
    --     -- traceM "SKIPPING"
    --     pure ctx
    --   _, _ → do
    --     -- traceM "REDRAWING BECAUSE HASHES ARE DIFFERENT"
    --     -- traceM ctx.hash
    --     -- traceM currentHash
    --     { hash: currentHash, pool: _, zoom: ctx.zoom } <$> renderLayers physicalDimensions ctx.zoom pool r
    -- where
    --   currentHash = Just <<<  hash $ (map drawHash above) /\ (map drawHash below) /\ (map drawHash workspace)

renderLayers ∷ Canvas.Dimensions → Zoom → Double.Pool → Render → Effect Double.Pool
renderLayers dimensions zoom pool { above, below, workspace } = do
  renderLayer dimensions zoom (Double.above pool) above
  renderLayer dimensions zoom (Double.below pool) below
  renderLayer dimensions zoom (Double.workspace pool) workspace
  Double.switch pool

renderLayer ∷ Canvas.Dimensions → Zoom → CanvasElement → Array Draw → Effect Unit
renderLayer { height, width } (Zoom zoom) canvas draws = do
  ctx ← Canvas.getContext2D canvas
  resetTransform ctx
  let
    Vector pan = zoom.pan
    t = (1.0 - 1.0 / zoom.ratio) / 2.0
  Canvas.scale ctx { scaleX: zoom.ratio, scaleY: zoom.ratio }
  Canvas.translate ctx { translateX: -width * (t + pan.x), translateY: -height * (t + pan.y) }

  -- height ← Canvas.getCanvasHeight canvas
  -- width ← Canvas.getCanvasWidth canvas
  let
    rectangle = { x: 0.0, y: 0.0, height, width }
  Canvas.clearRect ctx rectangle
  for_ draws (draw ctx)



