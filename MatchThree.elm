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
            seed = seed,
            time = 0
        }

unsupported states row col =
    let index row col = (row-1)*size + (col-1)
        state index = head (drop index states)
    in any (\x -> x == 0) (map (\row' -> state (index row' col)) [(row+1)..size])

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int) | Frame Time

update u world =
   case u of
        Click (row,col) ->
            let index = (row-1)*size + (col-1)
                states' = take index world.states ++ (0 :: drop (index+1) world.states)
            in {world | states <- states', time <- 0}
        Frame dt -> {world | time <- world.time + dt}
        _ -> world

-- Display

displaySquare world row col  =
    clickable (Signal.send locations (row,col)) (collage 20 20 [])

displayIcon world row col =
    let index = (row-1)*size + (col-1)
        x = toFloat (10+(col-1)*22)-((size*22-2)/2)
        y = ((size*22-2)/2) - (toFloat (10+(row-1)*22))
        state = head (drop index world.states)
        color state = head (drop (state-1) [green,blue,red,white])
        shape state = head (drop (state-1) [circle 10,ngon 3 10,rect 20 20,ngon 5 10])
        tumble = unsupported world.states row col
    in if state == 0 then toForm empty
       else move (x,y-(if tumble then world.time/10 else 0)) <| filled (color state) (shape state)

rowOfSquares world row =
    let squares = map (displaySquare world row) [1..size]
        between = spacer 2 2
    in flow right (intersperse between squares)

makeSquares world =
    let rows = map (rowOfSquares world) [1..size]
        between = spacer 2 2
    in flow down (intersperse between rows)

makeIcons world =
    let rowOfIcons world row = map (displayIcon world row) [1..size]
    in  collage (size*22-2) (size*22-2) (concatMap (rowOfIcons world) [1..size])

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