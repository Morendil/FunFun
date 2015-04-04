module Merlin where

import Graphics.Collage (..)
import Color (..)

import Signal
import Signal.Extra
import Window

-- Model

start (w,h) =
    {view={w=w,h=h}}

gridSize (w,h) = ((min w h)*2/3,(min w h)*2/3)

-- Update

update (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

display world =
    let backdrop = filled black <| rect (toFloat world.view.w) (toFloat world.view.h)
        (w,h) = gridSize (toFloat world.view.w,toFloat world.view.h)
        grid = outlined (solid white) (rect w h)
    in collage world.view.w world.view.h <| [backdrop,grid]

-- Signals

inputs = Window.dimensions

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
