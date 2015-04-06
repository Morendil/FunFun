module Merlin where

import Graphics.Collage (..)
import Color (..)

import List

import Mouse
import Signal
import Signal.Extra
import Window

-- Model

size = 5

start u =
    case u of
        Viewport (w,h) -> {
            view={w=w,h=h},
            states=List.map (always False) [1..size^2]
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
        Click coords -> updateClick coords world

updateClick (x,y) world =
    world

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
    in move ((small+2)*(frnd (size/2-row)),(small+2)*(frnd (size/2-col))) <| style (rect small small)

display world =
    let backdrop = filled black <| rect (toFloat world.view.w) (toFloat world.view.h)
        side = gridSize (toFloat world.view.w,toFloat world.view.h)
        grid = outlined (solid white) (rect side side)
        squares = List.concatMap (\row -> List.map (\col -> displaySquare row col world) [1..size]) [1..size]
    in collage world.view.w world.view.h <| List.concat [[backdrop,grid],squares]

-- Signals

mouseClicks = Signal.map Click (Signal.sampleOn Mouse.clicks Mouse.position)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,mouseClicks]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states