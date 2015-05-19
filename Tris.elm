module Tris where

import AnimationFrame exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Window
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h}
        }

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float

update u world = world

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
    in collage world.view.w world.view.h <| [backdrop, filled red <| rect 20 20]

-- Signals

dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states