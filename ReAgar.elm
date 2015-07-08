module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Generic exposing (..)
import List exposing (..)
import Color exposing (..)
import Random exposing (..)
import Text exposing (..)

import Debug
import Window
import Mouse
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> updateViewport (w,h) {
                view={w=0,h=0},
                players=[{x=0,y=0,dx=0,dy=0,mass=1000}],
                aim=(0,0)
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int)

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Point coords -> {world | aim <- addPair (-world.view.w//2,-world.view.h//2) coords}
        Frame dt ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
            in world
        _ -> world

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

radius mass = sqrt (mass / 3.14)
spacing = 40

displayMass cell =
    group [filled black <| circle (radius cell.mass), filled red <| circle ((radius cell.mass)-2),
                     text <| fromString <| toString <| cell.mass/100]

displayGrid world =
    let count = toFloat <| 2 * ((max world.view.w world.view.h) // (spacing*2))
        lines = group <|
                    map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-count/2..1+count/2] ++
                    map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-count/2..1+count/2]
    in lines

display world =
    let (Just player) = head world.players
        playerCells = collage world.view.w world.view.h <|
            map (\x -> move (x.x-player.x,x.y-player.y) <| displayMass x) world.players
        gridOffset coord = -(toFloat (floor coord % spacing))
        grid = collage world.view.w world.view.h [ move (gridOffset player.x,gridOffset player.y) <| displayGrid world]
    in layers [grid,playerCells]

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states