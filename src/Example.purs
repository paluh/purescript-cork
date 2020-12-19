module Example where

import Prelude

import Cork.Graphics.Canvas (TilesNumber(..))
import Cork.Graphics.Canvas.ImageData.Mutable.Filters.Grayscale (Mode(..)) as Grayscale
import Cork.Render (defaultStyle)
import Cork.Render.Zoom (Zoom(..))
import Cork.Sprites (drawImage, drawImageData, machine)
import Cork.Sprites.Caches.Machine (drawImagePerspective)
import Cork.Sprites.Sprite (externalImage, fromImageBitmap)
import Cork.Sprites.Sprite (grayscale, grayscaleToAlpha, stackedBlur) as Sprite
import Cork.Web.HTML.HTMLDocument (getHTMLElementById)
import Cork.Web.HTML.HTMLLoadedImageElement (Source(..)) as HTMLLoadedImageElement
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Geometry.Distance (unsafe) as Distance
import Geometry.Plane (point)
import Web.HTML (window)
import Web.HTML.Window (document)

type Hash = Int

kitten200x300 :: HTMLLoadedImageElement.Source
kitten200x300 = HTMLLoadedImageElement.URL "/static/kitten200x300.jpg"

kitten800x800 :: HTMLLoadedImageElement.Source
kitten800x800 = HTMLLoadedImageElement.URL "/static/kitten800x800.jpg"

main :: Effect Unit
main = do
  window >>= document >>= getHTMLElementById "app" >>= case _ of
    Just parent → do
      let
        dimensions =
          { physical: { height: Distance.unsafe 800.0, width: Distance.unsafe 800.0 }
          , logical: { height: Distance.unsafe 800.0, width: Distance.unsafe 800.0 }
          }
        p1 = point 0.0 0.0
        p2 = point 100.0 0.0
        p3 = point 0.0 350.0
        p4 = point 150.0 400.0
        p5 = point 300.0 450.0
        p6 = point 450.0 500.0
        p7 = point 600.0 550.0

        ib1 = externalImage kitten800x800
        ib2 = externalImage kitten200x300

        id1 = fromImageBitmap Nothing ib1
        id2 = fromImageBitmap Nothing ib2

        g1 = Sprite.grayscale Grayscale.Luminosity id1
        g2 = Sprite.grayscaleToAlpha Grayscale.Luminosity id2

        b1 = Sprite.stackedBlur 1 id1
        b2 = Sprite.stackedBlur 2 id1


        -- clip1 = clip
        --   ( List.Cons (Path.MoveAbs quad.first)
        --   $ List.Cons (Path.LineAbs quad.second)
        --   $ List.Cons (Path.LineAbs quad.third)
        --   $ List.Cons (Path.LineAbs quad.fourth)
        --   $ List.Cons Path.Close List.Nil
        --   )
        --   Antialiased
        --   (Just (BoundingBox.unsafe { x: 100.0, y: 200.0, height: 100.0, width: 100.0 }))
        --   ib1

        -- b1 = Sprite.stackedBlur 1 g1
        -- b2 = Sprite.stackedBlur 2 g1
        -- b3 = Sprite.stackedBlur 3 g1
        -- b4 = Sprite.stackedBlur 3 (Sprite.grayscaleToAlpha Grayscale.Luminosity i1)

        quad1 =
          { first: point 100.0 100.0
          , second: point 200.0 200.0
          , third: point 100.0 300.0
          , fourth: point 0.0 200.0
          }

        quad2 =
          { first: point 200.0 200.0
          , second: point 250.0 200.0
          , third: point 300.0 300.0
          , fourth: point 150.0 300.0
          }

        workspace =
          [ drawImage p1 defaultStyle ib1
          , drawImage p1 defaultStyle ib2
          , drawImageData p2 defaultStyle g1
          , drawImageData p3 defaultStyle g2
          , drawImageData p4 defaultStyle b1
          , drawImageData p5 defaultStyle b2
          , drawImagePerspective quad1 (TilesNumber 10) defaultStyle ib1
          , drawImagePerspective quad2 (TilesNumber 10) defaultStyle ib1
          ]

      m ← machine { dimensions }
      m.append parent
      m.render
        { done: Just $ const $ log "DONE"
        , scene:
          { above: [], below: [], workspace }
        , dimensions
        , zoom: Zoom { ratio: 1.0, pan: mempty }
        }
      -- m.render
      --   { done: Just $ const $ log "DONE"
      --   , scene: { above: [], below: [], workspace: [ di1, drawImageData p1 defaultStyle clip1 ] }
      --   , dimensions
      --   , zoom: Zoom { ratio: 1.0, pan: mempty }
      --   }
      m.run
    _ → log "App container with id \"#app\" not found"
