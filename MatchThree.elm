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

start u =
    case u of
        Viewport (w,h) -> {view={w=w,h=h}}

-- Update

type Update = Viewport (Int, Int) | Click (Int,Int)

update u world = world

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
    in collage world.view.w world.view.h [backdrop]

-- Signals

locations = Signal.channel (0,0)
buttons = Signal.map Click (Signal.subscribe locations)
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states