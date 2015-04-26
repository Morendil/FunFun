module Life (main) where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)

import Window
import Signal
import Signal.Extra

import Debug

-- Model

start u =
    case u of
        Viewport (w,h) -> {
            view={w=w,h=h},
            live=[(0,1),(1,0),(-1,-1),(0,-1),(1,-1)]
        }

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world =
    case u of 
        Viewport (w,h) ->
            let view' = {w=w,h=h}
            in { world | view <- view'}

-- Display

displayCell (row,col) =
    move (row*11,col*11) <| filled white <| rect 10 10

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
        cells = map displayCell world.live
    in collage world.view.w world.view.h (backdrop :: cells)

-- Signals

dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states