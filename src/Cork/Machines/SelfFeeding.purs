module Cork.Machines.SelfFeeding where

import Prelude

import Data.Const (Const)
import Data.Foldable (for_)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Map as Map
import Effect (Effect, foreachE)
import Effect.Ref as Ref
import Spork.Batch (Batch, unBatch)
import Spork.EventQueue (EventQueue, Loop(..))
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Spork.Transition (Transition)

type Update effects model action = model → action → Transition effects model action

-- | A specification for a Spork app:
-- |    * `render` - Renders a model to `Cork.Render.Render`
-- |    * `update` - Takes the current model and, with a new action, transitions to a new model while optionally running effects.
-- |    * `subs` - Determines the set of active subscriptions based on the model.
-- |    * `init` - Initial model and effects to kickstart the application.
type Machine effects subs model action =
  { update ∷ Update effects model action
  , subs ∷ model → Batch subs action
  , init ∷ Transition effects model action
  }

-- | A type synonym for Machines which don't have subs.
type BasicMachine effects model action = Machine effects (Const Void) model action

-- | The interface for communicating with a running Machine.
-- |    * `push` - Buffers an action to be run on the next tick.
-- |    * `run` - Initiates a tick of the Machine, flushing and applying all queued actions.
-- |    * `snapshot` - Yields the current model of the Machine.
-- |    * `restore` - Replaces the current model of the Machine.
-- |    * `subscribe` - Listens to Machine changes (model and actions).
type MachineInstance model action =
  { push ∷ action → Effect Unit
  , run ∷ Effect Unit
  , snapshot ∷ Effect model
  , restore ∷ model → Effect Unit
  , subscribe ∷ (MachineChange model action → Effect Unit) → Effect (Effect Unit)
  }

type MachineChange model action =
  { old ∷ model
  , action ∷ action
  , new ∷ model
  }

data MachineAction m q s i
  = Restore s
  | Action i
  | Interpret (Coproduct m q i)

type MachineState m q s i =
  { model ∷ s
  , interpret ∷ Loop Effect (Coproduct m q i)
  }

makeMachineQueue
  ∷ ∀ m q s i
  . (MachineChange s i → Effect Unit)
  → Interpreter Effect (Coproduct m q) i
  -- type Machine effects subs model action =
  → Machine m q s i
  → EventQueue Effect (MachineAction m q s i) (MachineAction m q s i)
makeMachineQueue onChange (Interpreter interpreter) app = EventQueue.withAccum \self → do
  let
    pushAction = self.push <<< Action
    pushEffect = self.push <<< Interpret <<< left

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
      ∷ MachineState m q s i
      → MachineAction m q s i
      → Effect (MachineState m q s i)
    update state@{ interpret: Loop k _ } = case _ of
      Interpret m → do
        nextInterpret ← k m
        pure $ state { interpret = nextInterpret }
      Action i → do
        let
          next = app.update state.model i
          nextState = state { model = next.model }
          appChange = { old: state.model, action: i, new: next.model }
        onChange appChange
        foreachE (unBatch next.effects) pushEffect
        pure nextState
      Restore nextModel → do
        let
          nextState = state { model = nextModel }
        pure nextState

    commit
      ∷ MachineState m q s i
      → Effect (MachineState m q s i)
    commit state = do
        tickInterpret ← runSubs state.interpret (unBatch (app.subs state.model))
        nextInterpret ← case tickInterpret of Loop _ f → f unit
        pure $ state { interpret = nextInterpret }

  interpret ← interpreter (self { push = self.push <<< Action })
  foreachE (unBatch app.init.effects) pushEffect

  let
    init =
      { model: app.init.model
      , interpret
      }
  pure { init, update, commit }

make
  ∷ ∀ effects subs model action
  . Interpreter Effect (Coproduct effects subs) action
  → Machine effects subs model action
  → Effect (MachineInstance model action)
make interpreter app = do
  subsRef ← Ref.new { fresh: 0, cbs: Map.empty }
  stateRef ← Ref.new app.init.model
  let
    handleChange ∷ MachineChange model action → Effect Unit
    handleChange appChange = do
      Ref.write appChange.new stateRef
      subs ← Ref.read subsRef
      for_ subs.cbs (_ $ appChange)

    subscribe'
      ∷ (MachineChange model action → Effect Unit)
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
    EventQueue.fix $ makeMachineQueue handleChange interpreter app

  pure
    { push: push <<< Action
    , snapshot: Ref.read stateRef
    , restore: push <<< Restore
    , subscribe: subscribe'
    , run
    }

