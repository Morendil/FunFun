module Life (main) where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)
import Time (..)

import Dict
import Set
import Window
import Mouse
import Keyboard
import Signal
import Signal.Extra
import Signal.Time

import Debug

-- Generic

cartesian fn list1 list2 =
    concatMap (\x -> map (fn x) list2) list1

addPair (x1,y1) (x2,y2) =
    (x1+x2,y1+y2)

-- Model

type Action = Live | Paused

start u =
    case u of
        Viewport (w,h) -> {
            view={w=w,h=h},
            live=[(0,1),(1,0),(-1,-1),(0,-1),(1,-1)],
            action = Live
        }

neighbors = let near = [0,-1,1] in drop 1 <| cartesian (,) near near

group list =
    let addToGroup pair dict = case Dict.get pair dict of
        Nothing -> Dict.insert pair 1 dict
        Just n -> Dict.insert pair (n+1) dict
    in foldr addToGroup Dict.empty list

step liveCells =
    let liveSet = Set.fromList liveCells
        allNeighbors = cartesian addPair liveCells neighbors
        grouped = group allNeighbors
        liveRules pair count = (count == 2 && Set.member pair liveSet) || (count == 3) 
        nextGen = Dict.filter liveRules grouped
    in Dict.keys nextGen

-- Update

type Update = Viewport (Int, Int) | Frame Float | Click (Int, Int) | Pause Bool

update u world =
    case u of 
        Viewport (w,h) ->
            let view' = {w=w,h=h}
            in { world | view <- view'}
        Pause p ->
            { world | action <- if world.action == Live then Paused else Live}
        Frame dt ->
            case world.action of
                Live -> { world | live <- step world.live }
                Paused -> world
        Click (x,y) ->
            let p = ((x-(world.view.w//2)-(size//2)) // (size+1),((world.view.h//2)-y-(size//2) ) // (size+1))
            in { world | live <- p :: world.live }

-- Display

size = 10

displayCell (row,col) =
    move (toFloat row*(size+1),toFloat col*(size+1)) <| filled white <| rect size size

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
        cells = map displayCell world.live
    in collage world.view.w world.view.h (backdrop :: cells)

-- Signals

keys = Signal.map Pause (Signal.Time.dropWithin (300 * millisecond) Keyboard.space)
clicks = Signal.map Click (Signal.sampleOn Mouse.clicks Mouse.position)
frames = Signal.map Frame (fps 5)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions, frames, clicks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states