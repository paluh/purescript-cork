module Cork.Sprites where

import Prelude

-- import Cork.Render (Draw, Render) as Render
-- import Cork.Sprites.Render (Draws, Draw)
-- import Cork.Sprites.Sprite (affAlg)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Matryoshka (cataM)

-- draw ∷ Draw → Aff Render.Draw
-- draw = traverse (cataM affAlg)
-- 
-- type State st =
--   { cache ∷ Cache
--   , draw ∷ Draw
--   | st
--   }
-- 
-- update ∷ ∀ st. Render.Context → Draws → Aff Render.Render
-- update renderContext
--   = map { above: [], below: [], workspace: _ }
--   <<< traverse draw
