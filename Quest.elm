module Quest where

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Text exposing (..)
import Transform2D

import Window
import Dict
import Array
import Signal
import Signal.Extra

import Debug

-- App imports
import Generic exposing (..)

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h},
                tiles = Array.fromList ([4]++(repeat (gridSize-2) 3)++[5]
                        ++ List.concat ( repeat (gridSize-2)
                            ([2]++(repeat (gridSize-2) 1)++[2])
                        ) ++
                        [6]++(repeat (gridSize-2) 3)++[7])
            }

tileFiles = Dict.fromList [
        (1,"quest/grass.png"),
        (2,"quest/roadNorth.png"),
        (3,"quest/roadEast.png"),
        (4,"quest/roadCornerES.png"),
        (5,"quest/roadCornerWS.png"),
        (6,"quest/roadCornerNE.png"),
        (7,"quest/roadCornerNW.png")
    ]

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world = world

-- Display

size = 70
gridSize = 9

depth (row,col) = row - col
        
square (row,col) = move (col*size,-row*size) <|
                    group [outlined (solid white) <| rect size size, text <|
                           Text.height 24 <| Text.color white <| fromString <| toString <| depth (row,col)]

applyAll = foldl Transform2D.multiply Transform2D.identity

grid fn =
    let positions = cartesian (,) [1..gridSize] [1..gridSize]
        depthSorted = sortBy depth positions
    in map fn depthSorted

matrix =
    let offset = Transform2D.translation (-gridSize*size/2) (gridSize*size/2)
        aroundHorizontal = Transform2D.matrix 1 0 0 (cos (degrees 60)) 0 -(sin (degrees 60))
        aroundNormal = Transform2D.matrix (cos (degrees 45)) -(sin (degrees 45)) (sin (degrees 45)) (cos (degrees 45)) 0 0
    in applyAll [offset, aroundNormal, aroundHorizontal]

placeTile (row,col) =
    let x = 50*(2-gridSize) + (row-1 + col-1) * 50
        y = 6 + (col-row) * 25
    in (x,y)

displayTile world xy =
    let (row,col) = xy
        index = (row-1)*gridSize+(col-1)
        (Just state) = Array.get index world.tiles
        (Just src) = Dict.get state tileFiles
    in move (placeTile xy) <| toForm (image 100 65 src)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
    in collage world.view.w world.view.h <|
        [backdrop,
         groupTransform matrix (grid square)]
         ++ grid (displayTile world)
         ++ [move (placeTile (1,2)) <| toForm (image 32 26 "quest/carRed4_002.png")]

-- Signals

locations = Signal.mailbox (0,0)
buttons = Signal.map Click locations.signal
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states