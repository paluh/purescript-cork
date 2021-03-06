module Cork.App
  ( App
  , AppInstance
  , AppChange
  , BasicApp
  , Config
  , make
  , module Spork.Batch
  , module Spork.Transition
  ) where


import Prelude

import Cork.Graphics.Canvas.CanvasElement (Dimensions, ImageRendering(..)) as CanvasElement
import Cork.Graphics.Canvas.Pool.Double (append, new, physicalDimensions, setDimensions, setImageRendering, setLogicalDimensions) as Double
import Cork.Render (Render, Context, render) as Render
import Cork.Render.Zoom (Zoom(..))
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Spork.Batch (Batch, unBatch)
import Spork.EventQueue (EventQueue, Loop(..))
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Spork.Transition (Transition)
import Unsafe.Reference (unsafeRefEq)
import Web.HTML (HTMLElement, window)
import Web.HTML.Window (requestAnimationFrame)

-- | A specification for a Spork app:
-- |    * `render` - Renders a model to `Cork.Render.Render`
-- |    * `update` - Takes the current model and, with a new action, transitions to a new model while optionally running effects.
-- |    * `subs` - Determines the set of active subscriptions based on the model.
-- |    * `init` - Initial model and effects to kickstart the application.
type App effects subs model action =
  { render ∷ model → Render.Render
  , update ∷ Render.Context → model → action → Transition effects model action
  , subs ∷ model → Batch subs action
  , init ∷ Transition effects model action
  }

type Config = { dimensions ∷ CanvasElement.Dimensions }

-- | A type synonym for Apps which don't have subs.
type BasicApp effects model action = App effects (Const Void) model action

-- | The interface for communicating with a running App.
-- |    * `push` - Buffers an action to be run on the next tick.
-- |    * `run` - Initiates a tick of the App, flushing and applying all queued actions.
-- |    * `snapshot` - Yields the current model of the App.
-- |    * `restore` - Replaces the current model of the App.
-- |    * `subscribe` - Listens to App changes (model and actions).
type AppInstance model action =
  { append ∷ HTMLElement → Effect Unit
  , push ∷ action → Effect Unit
  , run ∷ Effect Unit
  , snapshot ∷ Effect model
  , restore ∷ model → Effect Unit
  , setZoom ∷ Zoom → Effect Unit
  , setDimensions ∷ CanvasElement.Dimensions → Effect Unit
  , subscribe ∷ (AppChange model action → Effect Unit) → Effect (Effect Unit)
  }

type AppChange model action =
  { old ∷ model
  , action ∷ action
  , new ∷ model
  }

data AppAction m q s i
  = Action i
  | Append HTMLElement
  | Interpret (Coproduct m q i)
  | Render
  | Restore s
  | SetDimensions CanvasElement.Dimensions
  | SetZoom Zoom

data RenderStatus
  = NoChange
  | Pending
  | Flushed

derive instance eqRenderStatus ∷ Eq RenderStatus

type AppState m q s i =
  { model ∷ s
  , status ∷ RenderStatus
  , interpret ∷ Loop Effect (Coproduct m q i)
  , renderContext ∷ Render.Context
  }

makeImmediate ∷ Effect Unit → Effect Unit
makeImmediate run = void $ window >>= requestAnimationFrame run

makeAppQueue
  ∷ ∀ m q s i
  . (AppChange s i → Effect Unit)
  → Interpreter Effect (Coproduct m q) i
  -- type App effects subs model action =
  → App m q s i
  → Config
  → EventQueue Effect (AppAction m q s i) (AppAction m q s i)
makeAppQueue onChange (Interpreter interpreter) app cfg = EventQueue.withAccum \self → do
  let
    schedule = makeImmediate (self.push Render *> self.run)
    pushAction = self.push <<< Action
    pushEffect = self.push <<< Interpret <<< left

    nextStatus ∷ s → s → RenderStatus → RenderStatus
    nextStatus prevModel nextModel = case _ of
      NoChange
        | unsafeRefEq prevModel nextModel
        -- (Just <<<  hash $ (map drawHash above) /\ (map drawHash below) /\ (map drawHash workspace)
        → NoChange
        | otherwise → Pending
      Flushed → NoChange
      Pending → Pending

    runSubs
      ∷ Loop Effect (Coproduct m q i)
      → Array (q i)
      → Effect (Loop Effect (Coproduct m q i))
    runSubs interpret subs = do
      ref ← Ref.new interpret
      foreachE subs \sub -> do
        Loop k _ ← Ref.read ref
        next ← k (right sub)
        Ref.write next ref
      Ref.read ref

    update
      ∷ AppState m q s i
      → AppAction m q s i
      → Effect (AppState m q s i)
    update state@{ interpret: Loop k _ } = case _ of
      Interpret m → do
        nextInterpret ← k m
        pure $ state { interpret = nextInterpret }

      Action i → do
        let
          next = app.update state.renderContext state.model i
          status = nextStatus state.model next.model state.status
          nextState = state { model = next.model, status = status }
          appChange = { old: state.model, action: i, new: next.model }
        onChange appChange
        foreachE (unBatch next.effects) pushEffect
        pure nextState

      Append parent → do
        Double.append state.renderContext.pool parent
        pure $ state { status = Pending }

      Render → do
        renderContext' ← Render.render state.renderContext (app.render state.model)
        pure $ state { status = Flushed, renderContext = renderContext' }

      Restore nextModel → do
        let
          status = nextStatus state.model nextModel state.status
          nextState = state { model = nextModel, status = status }
        pure nextState

      SetDimensions dimensions → do
        physicalDimensions ← Double.physicalDimensions state.renderContext.pool
        if (physicalDimensions /= dimensions.physical)
          then do
            Double.setDimensions dimensions state.renderContext.pool
            pure $ state { status = Pending }
          else do
            Double.setLogicalDimensions dimensions.logical state.renderContext.pool
            pure state

      SetZoom zoom → pure $ if (zoom /= state.renderContext.zoom)
          then state { status = Pending, renderContext { zoom = zoom }}
          else state

    commit
      ∷ AppState m q s i
      → Effect (AppState m q s i)
    commit state = case state.status of
      Flushed →
        pure $ state { status = NoChange }
      status → do
        when (status == Pending) schedule
        tickInterpret ← runSubs state.interpret (unBatch (app.subs state.model))
        nextInterpret ← case tickInterpret of Loop _ f → f unit
        pure $ state { interpret = nextInterpret, status = NoChange }

  -- document ←
  --   DOM.window
  --     >>= DOM.document
  --     >>> map HTMLDocument.toDocument
  -- let
  --   vdomSpec = V.VDomSpec
  --     { document
  --     , buildWidget: buildThunk unwrap
  --     , buildAttributes: P.buildProp (\a → pushAction a *> self.run)
  --     }
  -- vdom ← EFn.runEffectFn1 (V.buildVDom vdomSpec) (unwrap (app.render app.init.model))
  -- void $ DOM.appendChild (Machine.extract vdom) el

  interpret ← interpreter (self { push = self.push <<< Action })
  foreachE (unBatch app.init.effects) pushEffect
  pool ← Double.new cfg.dimensions
  -- Double.setImageRendering CanvasElement.Pixelated pool
  let
    init =
      { model: app.init.model
      , status: NoChange
      , interpret
      , renderContext:
        { hash: Nothing
        , pool
        , zoom: Zoom { pan: mempty, ratio: 1.0 }
        }
      }
  pure { init, update, commit }

-- | Builds a running App given an `Interpreter` and a parent DOM Node.
-- |
-- | ```purescript
-- | example domNode = do
-- |   inst <- App.make (basicAff `merge` never) app domNode
-- |   _    <- inst.subscribe \_ -> log "Got a change!"
-- |   inst.run
-- | ```
-- |
-- | The returned `AppInstance` has yet to run any initial effects. You may
-- | use the opportunity to setup change handlers. Invoke `inst.run` when
-- | ready to run initial effects.
make
  ∷ ∀ effects subs model action
  . Interpreter Effect (Coproduct effects subs) action
  → App effects subs model action
  → Config
  → Effect (AppInstance model action)
make interpreter app cfg = do
  subsRef ← Ref.new { fresh: 0, cbs: Map.empty }
  stateRef ← Ref.new app.init.model
  let
    handleChange ∷ AppChange model action → Effect Unit
    handleChange appChange = do
      Ref.write appChange.new stateRef
      subs ← Ref.read subsRef
      for_ subs.cbs (_ $ appChange)

    subscribe'
      ∷ (AppChange model action → Effect Unit)
      → (Effect (Effect Unit))
    subscribe' cb = do
      subs ← Ref.read subsRef
      subsRef # Ref.write
        { fresh: subs.fresh + 1
        , cbs: Map.insert subs.fresh cb subs.cbs
        }
      pure (remove subs.fresh)

    remove ∷ Int → Effect Unit
    remove key =
      subsRef # Ref.modify_ \subs → subs
        { cbs = Map.delete key subs.cbs
        }

  { push, run } ←
    EventQueue.fix $ makeAppQueue handleChange interpreter app cfg

  pure
    { append: push <<< Append
    , push: push <<< Action
    , restore: push <<< Restore
    , run
    , snapshot: Ref.read stateRef
    , subscribe: subscribe'
    , setZoom: push <<< SetZoom
    , setDimensions: push <<< SetDimensions
    }

