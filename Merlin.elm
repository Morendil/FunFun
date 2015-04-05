module Merlin where

import Graphics.Collage (..)
import Color (..)

import List
import Signal
import Signal.Extra
import Window

-- Model

start (w,h) =
    {view={w=w,h=h}}

gridSize (w,h) = (min w h)*2/3
divide n side =
    let n' = toFloat n
    in (side-4-((n'-1)*2))/n'

-- Update

update (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

display world =
    let n = 5
        backdrop = filled black <| rect (toFloat world.view.w) (toFloat world.view.h)
        side = gridSize (toFloat world.view.w,toFloat world.view.h)
        grid = outlined (solid white) (rect side side)
        small = divide n side
        frnd = toFloat << round
        square row col = move ((small+2)*(frnd (n/2-row)),(small+2)*(frnd (n/2-col))) <| filled green <| rect small small
        squares = List.concatMap (\row -> List.map (\col -> square row col) [1..n]) [1..n]
    in collage world.view.w world.view.h <| List.concat [[backdrop,grid],squares]

-- Signals

inputs = Window.dimensions

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states
