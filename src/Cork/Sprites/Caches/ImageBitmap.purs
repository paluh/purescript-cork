module Cork.Sprites.Caches.ImageBitmap where

import Prelude

import Control.Monad.Error.Class (catchError)
import Cork.Graphics.Canvas.CanvasElement (new) as CanvasElement
import Cork.Graphics.Canvas.ImageBitmap (ImageBitmap)
import Cork.Graphics.Canvas.ImageBitmap (fromAnyImageData) as ImageBitmap
import Cork.Graphics.Canvas.ImageData (AnyImageData(..)) as ImageData
import Cork.Machines.SelfFeeding (Machine, Update, make) as SelfFeeding
import Cork.Sprites.Caches (Hash, Item) as Caches
import Cork.Sprites.Caches.Types (Cache, Hash, Item(..))
import Data.Map (insert) as Map
import Effect (Effect)
import Effect.AVar (new) as AVar.Effect
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Geometry.Plane.BoundingBox.Dimensions (unsafe) as Dimensions
import Graphics.Canvas (CanvasElement, ImageData)
import Spork.Batch (batch)
import Spork.Interpreter (basicAff)
import Spork.Interpreter (merge, never) as Interpreter

data Action
  = ProcessImageData Hash (Item ImageData)
  | CacheImageBitmap Hash (Item ImageBitmap)

type Model =
  { cache ∷ Cache ImageBitmap
  , workspace ∷ AVar CanvasElement
  }

type Machine = SelfFeeding.Machine Aff Array Model Action

update ∷ SelfFeeding.Update Aff Model Action
update model (ProcessImageData hash item) =
  let
    process = CacheImageBitmap hash <$> case item of
      Generated imageData → do
        let
          catch = flip catchError (const $ pure (ProcessingError "ImageData processing failed"))
        bracket (AVar.take model.workspace) (flip AVar.put model.workspace) $ \canvas → catch do
          imageBitmap ← ImageBitmap.fromAnyImageData canvas (ImageData.Immutable imageData)
          pure (Generated imageBitmap)
      (ProcessingError e) → pure (ProcessingError e)
    in
      { model, effects: batch [ process ] }
update model (CacheImageBitmap hash item) =
  { model: model { cache = Map.insert hash item model.cache }, effects: mempty }

type Change = { new ∷ { hash ∷ Hash, item ∷ Caches.Item ImageBitmap }, cache ∷ Cache ImageBitmap }

machine ∷ Effect
  { process ∷ Caches.Hash → Caches.Item ImageData → Effect Unit
  , subscribe :: (Change → Effect Unit) → Effect (Effect Unit)
  }
machine = do
  let
    raw = { height: 100.0, width: 100.0 }
    dimensions =
      { physical: Dimensions.unsafe raw
      , logical: Dimensions.unsafe raw
      }
  workspace ←  CanvasElement.new dimensions >>= AVar.Effect.new
  let
    model =
      { cache: mempty
      , workspace
      }
  i ← SelfFeeding.make
    (basicAff  (const $ pure unit) `Interpreter.merge` Interpreter.never)
    { update
    , subs: const $ mempty
    , init: { model, effects: mempty }
    }

  let
    subscribe handler = i.subscribe case _ of
      { action: CacheImageBitmap hash item, new: new } -> handler { new: { hash, item }, cache: new.cache }
      _ → pure unit
    process h imageData = do
      i.push (ProcessImageData h imageData)
      i.run

  pure { process, subscribe }
