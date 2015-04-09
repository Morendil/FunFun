module MatchThree where

import Graphics.Input (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)

import Window
import Signal
import Signal.Extra

import Debug

-- Model

size = 15

start u =
    case u of
        Viewport (w,h) -> {view={w=w,h=h}}

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world = world

-- Display

displaySquare row col world =
    let icon = collage 20 20 <| [filled blue (rect 20 20)]
    in clickable (Signal.send locations (row,col)) icon

rowOfSquares world row =
    let squares = map (\col -> displaySquare row col world) [1..size]
        between = spacer 2 2
    in flow right (intersperse between squares)

makeSquares world =
    let rows = map (rowOfSquares world) [1..size]
        between = spacer 2 2
    in flow down (intersperse between rows)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = collage world.view.w world.view.h [filled black <| rect w' h']
        squares = makeSquares world
    in layers [backdrop, container world.view.w world.view.h middle squares]

-- Signals

locations = Signal.channel (0,0)
buttons = Signal.map Click (Signal.subscribe locations)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states