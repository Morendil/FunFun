module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Window
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h}
        }

-- Update

type Update = Viewport (Int, Int)

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

display world =
    empty

-- Signals

dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states