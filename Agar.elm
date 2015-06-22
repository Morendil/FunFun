module Agar where

import AnimationFrame exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Generic exposing (..)
import List exposing (..)
import Color exposing (..)

import Debug
import Window
import Mouse
import Keyboard
import Signal.Extra

-- Model

start u =
    case u of
        Viewport (w,h) -> {
                view={w=w,h=h},
                pos={x=0,y=0},
                aim=(0,0)
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float | Point (Int, Int)

minx = -5000
maxx = 5000
miny = -5000
maxy = 5000

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Point coords -> {world | aim <- addPair (-world.view.w//2,-world.view.h//2) coords}
        Frame dt ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
            in glide world dt

glide world dt  =
    let (x,y) = (toFloat <| fst world.aim, toFloat <| snd world.aim)
        magnitude = sqrt (x^2+y^2)
        direction = (if magnitude < 10 then 0 else x/magnitude,if magnitude < 10 then 0 else y/magnitude)
        speed = (min 100 magnitude)/300
        (x',y') = (pin minx maxx (world.pos.x+(fst direction)*speed*dt),pin miny maxy (world.pos.y-(snd direction)*speed*dt))
        pos' = {x=x',y=y'}
    in {world | pos <- pos'}

updateViewport (w,h) world =
    let view = world.view
        view' = {view | w<-w,h<-h}
    in {world | view <- view'}

-- Display

display world =
    let spacing = 40
        radius = 20
        player = collage world.view.w world.view.h [filled black <| circle (radius+2), filled red <| circle radius]
        count = toFloat <| 2 * ((max world.view.w world.view.h) // (spacing*2))
        offset coord = -(toFloat (floor coord % spacing))
        lines = group <|
                    map (\k -> traced (dotted gray) <| segment (k*spacing,-1000) (k*spacing,1000)) [-count/2..1+count/2] ++
                    map (\k -> traced (dotted gray) <| segment (-1000,k*spacing) (1000,k*spacing)) [-count/2..1+count/2]
        grid = collage world.view.w world.view.h [ move (offset world.pos.x,offset world.pos.y) lines]
    in layers [grid,player]

-- Signals

frames = Signal.map Frame frame
cursors = Signal.map Point Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames,cursors]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states