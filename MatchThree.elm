module MatchThree where

import AnimationFrame (..)
import Time (..)

import Graphics.Input (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)

import Random

import Window
import Signal
import Signal.Extra

import Debug

-- Model

size = 15

start u =
    case u of
        Viewport (w,h) ->
        let (states, seed) = Random.generate (Random.list (size^2) (Random.int 1 4)) (Random.initialSeed 0)
        in {
            view={w=w,h=h},
            states = states,
            selected = -1,
            seed = seed,
            time = 0
        }

holes states row col =
    let index row col = (row-1)*size + (col-1)
        state index = head (drop index states)
    in length <| filter (\x -> x == 0) (map (\row' -> state (index row' col)) [(row+1)..size])

asRuns list = reverse <| asRuns' [] list

asRuns' runs list =
    case list of
        [] -> runs
        x :: xs -> case runs of
            [] -> asRuns' [(1,x)] xs
            (n,x') :: rs -> if x == x' then asRuns' ((n+1,x')::rs) xs
                                       else asRuns' ((1,x)::runs) xs

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Time

update u world =
   case u of
        Click (row,col) ->
            let index = (row-1)*size + (col-1)
            in {world | time <- 0, selected <- index}
        Frame dt -> {world | time <- world.time + dt}
        _ -> world

-- Display

iconSize = 25
iconSpc = 3
iconTotal = iconSize+iconSpc
iconRadius = iconSize/2

displaySquare world row col  =
    clickable (Signal.send locations (row,col)) (collage iconSize iconSize [])

displayIcon world row col =
    let index = (row-1)*size + (col-1)
        x =  (toFloat col-1)*iconTotal + iconRadius - (size*iconTotal-iconSpc)/2
        y = -(toFloat row-1)*iconTotal - iconRadius + (size*iconTotal-iconSpc)/2 - iconSpc
        state = head (drop index world.states)
        color state = head (drop (state-1) [green,blue,red,white])
        shape state = head (drop (state-1) [circle iconRadius,ngon 3 iconRadius,rect iconSize iconSize,ngon 5 iconRadius])
        tumble = (holes world.states row col) > 0
        to_y = y - (toFloat (holes world.states row col))*iconTotal + iconSpc
        now_y = y-(if tumble then world.time/10 else 0)
        icon = filled (color state) (shape state)
        frame = outlined (solid white) <| rect iconSize iconSize
    in if world.selected == index then move (x,y+iconSpc) <| toForm <| collage iconSize iconSize [icon, frame]
       else move (x,max to_y now_y) <| icon

rowOfSquares world row =
    let squares = map (displaySquare world row) [1..size]
        between = spacer iconSpc iconSpc
    in flow right (intersperse between squares)

makeSquares world =
    let rows = map (rowOfSquares world) [1..size]
        between = spacer iconSpc iconSpc
    in flow down (intersperse between rows)

makeIcons world =
    let rowOfIcons world row = map (displayIcon world row) [1..size]
    in  collage (size*iconTotal-iconSpc) (size*iconTotal-iconSpc) (concatMap (rowOfIcons world) [1..size])

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h [filled black <| rect w' h']
        squares = makeSquares world
        icons = makeIcons world
    in layers [backdrop, container world.view.w world.view.h middle icons, container world.view.w world.view.h middle squares]

-- Signals

locations = Signal.channel (0,0)

frames = Signal.map Frame frame
buttons = Signal.map Click (Signal.subscribe locations)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states