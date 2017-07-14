module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldMap, fold)
import Data.Int (toNumber)

import Color (Color, darken)
import Color.Scheme.MaterialDesign (blue, red, purple, pink, yellow, grey)
import Graphics.Isometric (Point, cube, filled, rotateZ, scale, renderScene,
                           prism, translateX, translateY, cone)
import Graphics.Isometric.Point as P
import Graphics.Isometric.DepthSort (depthSort)

import Math (sin, cos, pi)

import Signal.DOM (animationFrame, mousePos)

import Flare (numberSlider, lift, intSlider, color)
import Flare.Drawing as D




-- Cube

scene2 :: Number -> Number -> D.Drawing
scene2 rotZ time =
  D.translate 500.0 200.0 $
    renderScene { x: -4.0, y: -1.0, z: 3.0 } $
      scale 45.0 $ depthSort $ rotateZ rotZ $
           filled grey   (prism (P.point (-3.5) (-3.5) (-3.5)) 4.0 4.0 4.0)
         -- <> filled purple (prism (P.point (-1.0) pos1 0.0) 2.0 1.0 2.0)
         -- <> filled red    (prism (P.point pos2 (-1.0) 0.0) 1.0 2.0 2.0)
  where
    pos1 = 4.0 * cos (0.001 * time) - 0.5
    pos2 = 3.0 * sin (0.001 * time) - 0.5
    -- move = rotateZ 0.4 >>> translateY 1.1 >>> translateX 0.3





main = do
 

  D.runFlareDrawing "controls2" "canvas2" $
    scene2 <$> numberSlider "Rotationq"  0.0 (2.0 * pi) 0.1 0.0
           <*> lift animationFrame

 
