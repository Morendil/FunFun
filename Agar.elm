module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)

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

type Update = Viewport (Int, Int) | Frame Float

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
    let spacing = 40
        radius = 20
        player = collage world.view.w world.view.h [filled black <| circle (radius+2), filled red <| circle radius]
        grid = collage world.view.w world.view.h <|
                map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-500..500] ++
                map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-500..500]
    in layers [grid,player]

-- Signals

frames = Signal.map Frame frame
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states