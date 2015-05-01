module Quest where

import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Transform2D

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

size = 50
gridSize = 5

grid =
        map (\n -> traced (solid white) (segment (n*size,gridSize*size) (n*size,-gridSize*size))) [-gridSize..gridSize]
    ++  map (\n -> traced (solid white) (segment (-gridSize*size,n*size) (gridSize*size,n*size))) [-gridSize..gridSize]

matrix =
    let aroundHorizontal = Transform2D.matrix 1 0 0 (cos (degrees 45)) 0 -(sin (degrees 45))
        aroundNormal = Transform2D.matrix (cos (degrees 45)) -(sin (degrees 45)) (sin (degrees 45)) (cos (degrees 45)) 0 0
    in Transform2D.multiply aroundHorizontal aroundNormal

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        backdrop = filled black <| rect w' h'
    in collage world.view.w world.view.h [backdrop, (groupTransform matrix grid)]

-- Signals

locations = Signal.mailbox (0,0)
buttons = Signal.map Click locations.signal
dimensions = Signal.map Viewport (Window.dimensions)
inputs = Signal.mergeMany [dimensions,buttons]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states