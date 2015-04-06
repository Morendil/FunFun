module Merlin where

import Graphics.Input (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)

import List
import Mouse
import Window
import Signal
import Signal.Extra

import Debug

-- Model

size = 5

start u =
    case u of
        Viewport (w,h) -> {
            view={w=w,h=h},
            states=List.map (always True) [1..size^2]
        }

gridSize (w,h) = (min w h)*2/3
divide n side =
    let n' = toFloat n
    in (side-4-((n'-1)*2))/n'

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world =
    case u of
        Viewport vp -> updateViewport vp world
        Click square -> updateClick square world

updateClick (row,col) world =
    let which = ((row-1)*size)+col-1
        offsets = List.filter (\cand -> cand >= 0 && cand < size^2) (List.map (\ offset -> which+offset) [-5,-1,0,1,5])
        toggle which states =
            let before = List.take which states
                after = List.drop which states
            in before ++ (not (List.head after) :: (List.tail after))
    in {world | states <- List.foldl toggle world.states offsets}

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

displaySquare row col world =
    let side = gridSize (toFloat world.view.w,toFloat world.view.h)
        small = divide size side
        frnd = toFloat << round
        state = List.head (List.drop ((((floor row)-1)*size)+(floor col)-1) world.states)
        style = if state then filled green else outlined (solid white)
        disp = collage (floor small) (floor small) <| [style (rect small small)]
    in clickable (Signal.send clicks (row,col)) disp

rowOfSquares world row =
    let squares = List.map (\col -> displaySquare row col world) [1..size]
        between = spacer 2 2
    in flow right (List.intersperse between squares)

makeSquares world =
    let rows = List.map (rowOfSquares world) [1..size]
        between = spacer 2 2
    in flow down (List.intersperse between rows)

display world =
    let backdrop = filled black <| rect (toFloat world.view.w) (toFloat world.view.h)
        side = gridSize (toFloat world.view.w,toFloat world.view.h)
        grid = outlined (solid white) (rect side side)
        table = collage world.view.w world.view.h [backdrop,grid]
        squares = List.concatMap (\row -> List.map (\col -> displaySquare row col world) [1..size]) [1..size]
    in layers [table, container world.view.w world.view.h middle (makeSquares world)]

-- Signals

clicks = Signal.channel (0,0)
squareClicks = Signal.map Click (Signal.subscribe clicks)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,squareClicks]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states