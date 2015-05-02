module Quest where

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Text exposing (..)
import Transform2D

import Window
import Signal
import Signal.Extra

import Debug

-- Model

start u =
    case u of
        Viewport (w,h) -> {view={w=w,h=h}}

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world = world

-- Display

size = 70
gridSize = 6

square row col = move (col*size,-row*size) <|
                    group [outlined (solid white) <| rect size size, text <|
                           Text.height 24 <| Text.color white <| fromString <| toString (row,col)]

applyAll = foldl Transform2D.multiply Transform2D.identity
grid fn = concatMap (\row -> map (\ col -> fn row col) [1..gridSize]) [1..gridSize]

matrix =
    let offset = Transform2D.translation (-gridSize*size/2) (gridSize*size/2)
        aroundHorizontal = Transform2D.matrix 1 0 0 (cos (degrees 60)) 0 -(sin (degrees 60))
        aroundNormal = Transform2D.matrix (cos (degrees 45)) -(sin (degrees 45)) (sin (degrees 45)) (cos (degrees 45)) 0 0
    in applyAll [offset, aroundNormal, aroundHorizontal]

placeTile row col =
    let x = -200 + (row-1 + col-1) * 50
        y = 6 + (col-row) * 25
    in (x,y)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
    in collage world.view.w world.view.h
        [backdrop,
         groupTransform matrix (grid square),
         move (placeTile 1 1) <| toForm (image 100 65 "quest/grass.png"),
         move (placeTile 2 2) <| toForm (image 100 65 "quest/grass.png"),
         move (placeTile 3 3)  <| toForm (image 100 65 "quest/grass.png"),
         move (placeTile 2 1) <| toForm (image 100 65 "quest/grass.png")]

-- Signals

locations = Signal.mailbox (0,0)
buttons = Signal.map Click locations.signal
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states