module Cork.Svg
  ( module Types
  , render
  , render'
  )
  where

import Prelude

import Cork.Svg.Attributes (viewBox) as Attributes
import Cork.Svg.Types (Document(..), Render(..))
import Cork.Svg.Types (Document(..), Render(..)) as Types
import Data.Maybe (Maybe(..))
import Geometry (Distance(..))
import Geometry.Distance (ConversionFactor)
import Geometry.Distance (convert) as Distance
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Seegee.Geometry.Distance.Units (Pixel, Scene) as Units
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (Attribute, attribute) as M
import Text.Smolder.Renderer.String (render) as Smolder
import Text.Smolder.SVG (defs, svg) as M
import Text.Smolder.SVG.Attributes (baseProfile, height, version, viewBox, width) as A

-- | XXX: We want to drop Smoler dependency probably
-- | and move this all helpers to Units.React.Basic.DOM.Svg
xmlns :: String -> M.Attribute
xmlns = M.attribute "xmlns"

render ∷ ∀ e. Document e → Maybe (ConversionFactor Units.Scene Units.Pixel) → Render
render (Document { body, defs, viewBox: viewBox@(BoundingBox viewBoxV) }) mRatio = Render $ document
  where
    document = Smolder.render $ M.svg
      ! A.viewBox (Attributes.viewBox viewBox)
      ! case mRatio of
          Nothing → A.width ("100%")
          Just ratio →
            let
              Distance height = Distance.convert ratio viewBoxV.height
              Distance width = Distance.convert ratio viewBoxV.width
            in
              A.height (show height) <> A.width (show width)
      ! A.baseProfile "full"
      ! xmlns "http://www.w3.org/2000/svg"
      ! A.version "1.1"
      $ do
        M.defs defs
        body

render' ∷ ∀ e. Document e → Render
render' d = render d Nothing
