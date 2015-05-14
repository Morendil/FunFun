module Quest where

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Text exposing (..)
import AnimationFrame exposing (..)

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

type Phase = Run | Crash
type Direction = East | South | West | North

start u =
    case u of
        Viewport (w,h) -> zero {
                view={w=w,h=h},
                tiles = Array.fromList ([4]++(repeat (gridSize-3) 3)++[0,5]
                        ++ List.concat ( repeat (gridSize-2)
                            ([2]++(repeat (gridSize-2) 1)++[2])
                        ) ++
                        [6]++(repeat (gridSize-2) 3)++[7]),
                phase = Run,
                start = (0,0),
                car = (0,0),
                dest = (0,0),
                when = 0,
                dir = West,
                time = 0,
                anim = 0
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

carImage direction = case direction of
        West -> "quest/carRed4_002.png"
        South -> "quest/carRed4_007.png"
        East -> "quest/carRed4_006.png"
        North -> "quest/carRed4_000.png"

zero world = {world |
    start <- placeTile (1,1), dest <- placeTile (1,gridSize), dir <- West, time <- 0, when <- 300, anim <- 1}
one world = {world |
    start <- placeTile (1,gridSize), dest <- placeTile (gridSize,gridSize), dir <- South, time <- 0, when <- 300, anim <- 2}
two world = {world |
    start <- placeTile (gridSize,gridSize), dest <- placeTile (gridSize,1), dir <- East, time <- 0, when <- 300, anim <- 3}
three world = {world |
    start <- placeTile (gridSize,1), dest <- placeTile (1,1), dir <- North, time <- 0, when <- 300, anim <- 0}

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float

update u world =
    case u of
        Frame dt ->
            if world.time > world.when then
                (case world.anim of
                    0 -> zero
                    1 -> one
                    2 -> two
                    3 -> three) world
            else
            let (carx,cary) = world.car
                (srtx,srty) = world.start
                (dstx,dsty) = world.dest
                car' = (srtx+(dstx-srtx)*((min world.time world.when)/world.when),srty+(dsty-srty)*((min world.time world.when)/world.when))
            in {world | car <- car', time <- world.time + dt/10}
        _ -> world

addPair (x1,y1) (x2,y2) =
    (x1+x2,y1+y2)

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
    in case (Dict.get state tileFiles) of
        Just src -> move (placeTile xy) <| toForm (image 100 65 src)
        _ -> group []

overlayStyle = { typeface = [], height = Just 24, color = white, bold = True, italic = False, line = Nothing}

overlay world =
    if world.phase == Crash then [container world.view.w world.view.h middle <| centered <| style overlayStyle <| fromString "You Crashed!"]
    else []

displayCar world =
    let img = carImage world.dir
    in move (addPair world.car (6,12)) <| toForm (image 32 26 img)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
        game = collage world.view.w world.view.h <|
        [backdrop,
         groupTransform matrix (grid square)]
         ++ grid (displayTile world)
         ++ [move (placeTile (5,-1)) <| toForm (image 100 65 "quest/roadEast.png")]
         ++ [displayCar world]
    in layers <| game :: (overlay world)

-- Signals

locations = Signal.mailbox (0,0)
buttons = Signal.map Click locations.signal
dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,buttons,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states