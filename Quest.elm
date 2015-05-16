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
import Mouse

import Random
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
        Viewport (w,h) -> {
                view={w=w,h=h},
                tiles = Array.fromList ([4]++(repeat (gridSize-3) 3)++[0,5]
                        ++ List.concat ( repeat (gridSize-2)
                            ([2]++(repeat (gridSize-2) 1)++[2])
                        ) ++
                        [6]++(repeat (gridSize-2) 3)++[7]),
                next = 3,
                seed = Random.initialSeed 0,
                phase = Run,
                start = (1,1),
                dest = (1,2),
                car = placeTile (1,1),
                when = 50,
                dir = East,
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
        East -> "quest/carRed4_002.png"
        South -> "quest/carRed4_007.png"
        West -> "quest/carRed4_006.png"
        North -> "quest/carRed4_000.png"

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float

roadAt tile world =
    let (row,col) = tile
        index = (row-1)*gridSize+(col-1)
        (Just value) = Array.get index world.tiles
    in value

advance world =
    case world.dir of
        East -> {world | dest <- addPair world.dest (0,1)} 
        West -> {world | dest <- addPair world.dest (0,-1)}
        South -> {world | dest <- addPair world.dest (1,0)}
        North -> {world | dest <- addPair world.dest (-1,0)}

arriveAt tile world =
    let road = roadAt tile world
        world' = {world | time <-0, start <- world.dest}
    in case road of
        3 -> if (world.dir == East || world.dir == West) then advance world'
             else {world | phase <- Crash}
        2 -> if (world.dir == North || world.dir == South) then advance world'
             else {world | phase <- Crash}
        4 -> if (world.dir == North) then advance {world' | dir <- East}
             else {world | phase <- Crash}
        5 -> if (world.dir == East) then advance {world' | dir <- South}
             else {world | phase <- Crash}
        6 -> if (world.dir == West) then advance {world' | dir <- North}
             else {world | phase <- Crash}
        7 -> if (world.dir == South) then advance {world' | dir <- West}
             else {world | phase <- Crash}
        _ -> {world | phase <- Crash}

update u world =
    case u of
        Viewport vp -> updateViewport vp world
        Click coords ->
            let tile = whichTile world coords
                (row,col) = tile
                index = (row-1)*gridSize+(col-1)
                (next,seed) = Random.generate (Random.int 1 7) world.seed
            in {world | tiles <- Array.set index world.next world.tiles, seed <- seed, next <- next}
        Frame dt ->
            if world.time > world.when then
                arriveAt world.dest world
            else
            let (carx,cary) = world.car
                (srtx,srty) = placeTile world.start
                (dstx,dsty) = placeTile world.dest
                car' = (srtx+(dstx-srtx)*((min world.time world.when)/world.when),srty+(dsty-srty)*((min world.time world.when)/world.when))
            in {world | car <- car', time <- world.time + dt/10}
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

addPair (x1,y1) (x2,y2) =
    (x1+x2,y1+y2)

-- Display

whichTile world mousePos =
    let (rx,ry) = addPair mousePos (-(world.view.w-gridSize*100-85)//2,-world.view.h//2+10)
    in ((rx+ry*2)//100,(rx-ry*2)//100)

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
    in renderTile xy state

renderTile xy state =
    case (Dict.get state tileFiles) of
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
         ++ [renderTile (5,-1) world.next]
         ++ [displayCar world]
    in layers <| game :: (overlay world)

-- Signals

clicks = Signal.map Click (Signal.sampleOn Mouse.clicks Mouse.position)
dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,clicks,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states