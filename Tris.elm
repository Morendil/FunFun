module Tris where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Window
import Signal.Extra

import Array

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h},
                board = Array.fromList (List.repeat (height*width) 0),
                piece = Array.fromList [1],
                coords = (width//2,height-1),
                speed = 500,
                time = 0
        }

height = 20
width = 10
size = 20

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float

update u world = 
    case u of
        Frame dt ->
            let world' = {world | time <- world.time + dt}
            in if world'.time > world.speed then
                {world' | coords <- (fst world.coords, max 1 (snd world.coords-1)), time <- 0}
            else world'
        _ -> world


-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h <| [filled black <| rect w' h']
        (x,y) = (toFloat (fst world.coords), toFloat (snd world.coords))
        items = [move (x*size-(width*size)/2,y*size-(height*size)/2-size/2) <| filled red <| rect size size]
        board = container world.view.w world.view.h middle <| collage (width*size) (height*size) items
    in  layers [backdrop, board]

-- Signals

dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states