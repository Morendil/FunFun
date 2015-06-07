module Tris where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Maybe exposing (withDefault)
import List exposing (..)
import Color exposing (..)
import Text exposing (..)
import Generic exposing (..)

import Random exposing (..)
import Window
import Keyboard
import Signal.Extra

import Array

-- Model

start u =
    case u of
        Viewport (w,h) -> nextPiece {
                view={w=w,h=h},
                board = Array.fromList (List.repeat (height*width) 0),
                piece = [],
                coords = (width//2,height-1),
                done = False,
                speed = 500,
                time = 0,
                seed = initialSeed 0,
                level = 1,
                lines = 0,
                score = 0
        }

height = 20
width = 10
size = 20

tetrominoes = [
    {shape=[(-2,0),(-1,0),(0,0),(1,0)],color=1}, -- I
    {shape=[(-1,1),(-1,0),(0,0),(1,0)],color=2}, -- J
    {shape=[(-1,0),(0,0),(1,0),(1,1)],color=3}, -- L
    {shape=[(-1,0),(-1,1),(0,0),(0,1)],color=4}, -- O
    {shape=[(-1,0),(0,0),(0,1),(1,1)],color=5}, -- S
    {shape=[(-1,0),(0,0),(0,1),(1,0)],color=6}, -- T
    {shape=[(-1,1),(0,0),(0,1),(1,0)],color=7}] -- Z

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Float | Control {x:Int, y:Int}

nextPiece world =
    let (piece', seed') = generate tetrominoGen world.seed
        done' = not (valid piece'.shape world.board (width//2,height-1))
    in if done' then {world | done <- True}
        else {world | coords <- (width//2,height-1), piece <- piece', seed <- seed'}

picker list seed =
    let (index,seed') = generate (int 0 ((length list) - 1)) seed
        (Just value) = Array.get index (Array.fromList list)
    in (value, seed')

tetrominoGen = customGenerator <| picker tetrominoes

valid piece board (x',y') =
    let hit (ox,oy) =
            let value = Array.get (ox+x'-1+(oy+y'-1)*width) board
            in not (value == Just 0)
        out (ox,oy) =
            (ox+x' < 1) || (ox+x' > width) ||
            (oy+y' < 1) || (oy+y' > height)
    in not ((any hit piece) || (any out piece))

fall (x,y) = (x, y-1)
shift keys (x,y) = (x+keys.x,y+(min 0 keys.y))

rotate (x,y) = (y,-x)
rotatePiece world =
    let piece = world.piece
        shape' = map rotate piece.shape
        piece' = {piece | shape <- shape'}
        maybeWallkick = map (\by -> shift {x=by,y=0} world.coords) [0,-1,1]
        retain = head <| filter (valid shape' world.board) maybeWallkick
    in case retain of
        Just placed -> {world | piece <- piece', coords <- placed}
        Nothing -> world

apply movement world = 
    let coords' = movement world.coords
        ok = valid world.piece.shape world.board coords'
    in if ok then {world | coords <- coords'} else world

transfer board (x,y) piece =
    let transferTile (ox,oy) board = Array.set (ox+x-1+(oy+y-1)*width) piece.color board
    in foldr transferTile board piece.shape

freeze world =
    {world | board <- transfer world.board world.coords world.piece}

row board r =
    Array.slice (r*width) ((r+1)*width) board

incomplete row =
    any (\x -> x == 0) (Array.toList row)

score count world =
    let value = withDefault 800 <| Array.get count (Array.fromList [0,100,300,500,800])
        lineValue = withDefault 800 <| Array.get count (Array.fromList [0,1,3,5,8])
        speed' = withDefault 1 <| Array.get world.level (Array.fromList [48,48,43,38,33,28,23,18,13,8,6,5,4,3,2,1])
        lines' = world.lines + lineValue
        score' = world.score + value * world.level
        level' = 1 + (world.lines-1)//5
    in {world | lines <- lines', score <- score' , speed <- speed' * 1000 / 60, level <- level'}

clean world =
    let cleaned = filter incomplete <| map (row world.board) [0..(height-1)]
        count = height - (length cleaned)
        start = foldr Array.append Array.empty <| cleaned
        board' = Array.append start (Array.repeat (width * count) 0)
        world' = {world | board <- board'}
    in if count == 0 then world' else score count world'

drop world =
    let world' = apply fall {world | time <- 0}
        stopped = world.coords == world'.coords
    in if stopped then nextPiece <| clean <| freeze world'
    else world'

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Frame dt ->
            let world' = {world | time <- world.time + dt}
            in if world'.time > world'.speed then drop world'
            else world'
        Control keys ->
            if keys.y > 0 then rotatePiece world
            else apply (shift keys) world
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

displayTile coords kind offset =
    let (ox,oy) = offset
        (x,y) = addPair coords (toFloat ox, toFloat oy)
        (Just color) = Array.get (kind-1) (Array.fromList [red,orange,yellow,green,blue,purple,brown])
    in move (x*size-(width*size)/2-size/2,y*size-(height*size)/2-size/2) <| filled color <| rect (size-0.5) (size-0.5)

displayPiece world =
    let piece = (toFloat (fst world.coords), toFloat (snd world.coords))
    in map (displayTile piece world.piece.color) world.piece.shape

displayBoard world = 
    let board = world.board
        valueAt index = let (Just value) = Array.get index board in value
        tiles = filter (\i -> (valueAt i) > 0) [0..(height*width)-1]
    in map (\index -> displayTile (toFloat (index%width)+1,toFloat (index//width)+1) (valueAt index) (0,0)) tiles

overlayStyle = { typeface = [], height = Just 24, color = white, bold = True, italic = False, line = Nothing}

infoText = leftAligned << style overlayStyle << fromString

overlay world =
    let gameStatus = if world.done then "Game over!" else ""
    in [
        container world.view.w world.view.h middle <|
        (container (size*12) (size*32) topLeft <|
                (flow down <| map infoText [ ("Level: "++(toString world.level)),
                                             ("Lines: "++(toString world.lines)),
                                             ("Score: "++(toString world.score)),
                                             (gameStatus)])
            ),
        container world.view.w world.view.h middle <|
        (container (4+size*10) (4+size*20) topLeft <| collage (4+size*10) (4+size*20) [outlined (solid white) <| rect (4+size*10) (4+size*20)])
    ]

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h <| [filled black <| rect w' h']
        items = displayPiece world ++ displayBoard world
        board = container world.view.w world.view.h middle <| collage (width*size) ((height+6)*size) items
    in  layers <| [backdrop, board] ++ overlay world

-- Signals

controls = Signal.map Control Keyboard.arrows
dimensions = Signal.map Viewport (Window.dimensions)
frames = Signal.map Frame frame

inputs = Signal.mergeMany [dimensions,frames,controls]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states