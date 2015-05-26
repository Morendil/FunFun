module Tris where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Generic exposing (..)

import Window
import Keyboard
import Signal.Extra

import Array

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h},
                board = Array.fromList (List.repeat (height*width) 0),
                piece = [(0,0),(0,1),(0,2)],
                coords = (width//2,height-1),
                speed = 500,
                time = 0
        }

height = 20
width = 10
size = 20

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float | Control {x:Int, y:Int}

constrain _ _ (x,y) = (pin x 1 width, pin y 1 height)
pin x low high = min (max low x) high

fall (x,y) = (x, y-1)
shift keys (x,y) = (x+keys.x,y+(min 0 keys.y))

rotatePiece = identity

apply movement world = 
    let coords' = constrain world.piece world.board (movement world.coords)
    in {world | coords <- coords'}

transfer board (x,y) piece =
    let transferTile (ox,oy) board = Array.set (ox+x-1+(oy+y-1)*width) 1 board
    in foldr transferTile board piece

freeze world =
    {world | coords <- (width//2,height-1), piece <- [(0,0)], board <- transfer world.board world.coords world.piece}

drop world =
    let world' = apply fall {world | time <- 0}
    in if world.coords == world'.coords then freeze world'
    else world'

update u world = 
    case u of
        Frame dt ->
            let world' = {world | time <- world.time + dt}
            in if world'.time > world'.speed then drop world'
            else world'
        Control keys ->
            if keys.y > 0 then rotatePiece world
            else apply (shift keys) world
        _ -> world

-- Display

displayTile coords offset =
    let (ox,oy) = offset
        (x,y) = addPair coords (toFloat ox, toFloat oy)
    in move (x*size-(width*size)/2-size/2,y*size-(height*size)/2-size/2) <| filled red <| rect (size-0.5) (size-0.5)

displayPiece world =
    let piece = (toFloat (fst world.coords), toFloat (snd world.coords))
    in map (displayTile piece) world.piece

displayBoard world = 
    let board = world.board
        valueAt index = let (Just value) = Array.get index board in value
        tiles = filter (\i -> (valueAt i) > 0) [0..(height*width)-1]
    in map (\index -> displayTile (toFloat (index%width)+1,toFloat (index//width)+1) (0,0)) tiles

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h <| [filled black <| rect w' h']        
        items = displayPiece world ++ displayBoard world
        board = container world.view.w world.view.h middle <| collage (width*size) ((height+6)*size) items
    in  layers [backdrop, board]

-- Signals

controls = Signal.map Control Keyboard.arrows
dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,frames,controls]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states