module Quest where

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Text exposing (..)
import AnimationFrame exposing (..)

import Transform2D
import Transform2D.Extra exposing (..)
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
        Viewport (w,h) -> advance {
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
                dest = (1,1),
                car = placeTile (1,1),
                when = 50,
                dir = East,
                time = 0,
                level = 1,
                goal = 32
            }

tileFiles = Dict.fromList [
        (1,"quest/grass.png"),
        (2,"quest/roadNorth.png"),
        (3,"quest/roadEast.png"),
        (4,"quest/roadCornerES.png"),
        (5,"quest/roadCornerWS.png"),
        (6,"quest/roadCornerNE.png"),
        (7,"quest/roadCornerNW.png"),
        (8,"quest/crossroad.png"),
        (9,"quest/roadTWest.png"),
        (10,"quest/roadTSouth.png"),
        (11,"quest/roadTEast.png"),
        (12,"quest/roadTNorth.png")
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

sw pick dir = case dir of
    South -> East
    West -> North
ne pick dir = case dir of
    North -> West
    East -> South
se pick dir = case dir of
    South -> West
    East -> North
nw pick dir = case dir of
    North -> East
    West -> South
nse pick dir = case dir of
    East -> from [North,South] pick
    _ -> dir
ewn pick dir = case dir of
    North -> from [East,West] pick
    _ -> dir
nsw pick dir = case dir of
    West -> from [North,South] pick
    _ -> dir
sew pick dir = case dir of
    South -> from [East,West] pick
    _ -> dir

from array index =
    let len = List.length array
        (Just value) = Array.get (index % len) (Array.fromList array)
    in value

rules = Dict.fromList [
        (2,{dirs=[North,South],turn=always identity}),
        (3,{dirs=[East,West],turn=always identity}),
        (4,{dirs=[North,West],turn=nw}),
        (5,{dirs=[North,East],turn=ne}),
        (6,{dirs=[South,West],turn=sw}),
        (7,{dirs=[South,East],turn=se}),
        (8,{dirs=[South,East,North,West],turn=always identity}),
        (9,{dirs=[North,South,East],turn=nse}),
        (10,{dirs=[East,West,North],turn=ewn}),
        (11,{dirs=[North,South,West],turn=nsw}),
        (12,{dirs=[South,East,West],turn=sew})
    ]

arriveAt tile world =
    let road = roadAt tile world
        world' = {world | time <-0, start <- world.dest}
    in case Dict.get road rules of
            Just rule ->
                let found = any (\x -> world.dir == x) rule.dirs
                    (choice,seed) = Random.generate (Random.int 0 3) world.seed
                in if found then advance {world' | dir <- rule.turn choice world.dir, seed <- seed, goal <- max 0 (world.goal-1)}
                   else {world' | phase <- Crash }
            Nothing -> {world' | phase <- Crash }

update u world =
    case u of
        Viewport vp -> updateViewport vp world
        Click coords ->
            let tile = whichTile world coords
                (row,col) = tile
                index = (row-1)*gridSize+(col-1)
                (next,seed) = Random.generate (Random.int 2 12) world.seed
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

grid fn =
    let positions = cartesian (,) [1..gridSize] [1..gridSize]
        depthSorted = sortBy depth positions
    in map fn depthSorted

applyAll = List.foldl Transform2D.multiply Transform2D.identity

matrix =
    let offset = Transform2D.translation (-gridSize*size/2) (gridSize*size/2)
        aroundHorizontal = Transform2D.matrix 1 0 0 (cos (degrees 60)) 0 -(sin (degrees 60))
        aroundNormal = Transform2D.matrix (cos (degrees 45)) -(sin (degrees 45)) (sin (degrees 45)) (cos (degrees 45)) 0 0
    in applyAll [offset, aroundNormal, aroundHorizontal]

placeTile (row,col) = transform matrix (col*size,-row*size)

displayTile world xy =
    let (row,col) = xy
        index = (row-1)*gridSize+(col-1)
        (Just state) = Array.get index world.tiles
    in renderTile xy state

renderTile xy state =
    case Dict.get state tileFiles of
        Just src -> move (placeTile xy) <| toForm (image 100 65 src)
        _ -> group []

overlayStyle = { typeface = [], height = Just 24, color = white, bold = True, italic = False, line = Nothing}

overlay world =
    [
        container world.view.w world.view.h middle <|
        (container (gridSize*100) (gridSize*65) topLeft <|
                (flow down [leftAligned <| style overlayStyle <| fromString ("Level: "++(toString world.level)),
                            leftAligned <| style overlayStyle <| fromString ("To go: "++(toString world.goal))])
            )
    ] ++
    if world.phase == Crash then [container world.view.w world.view.h middle <| centered <| style overlayStyle <| fromString "You Crashed!"]
    else []

displayCar world =
    let img = carImage world.dir
    in move (addPair world.car (6,12)) <| toForm (image 32 26 img)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
        game = collage world.view.w world.view.h <|
        backdrop ::
        (grid (displayTile world)
            ++ [renderTile (5,-1) world.next]
            ++ [displayCar world])
    in layers <| game :: (overlay world)

-- Signals

clicks = Signal.map Click (Signal.sampleOn Mouse.clicks Mouse.position)
dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,clicks,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states