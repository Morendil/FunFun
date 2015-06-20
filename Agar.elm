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
                pos={x=0,y=0}
        }

-- Update

type Update = Viewport (Int, Int) | Frame Float (Int, Int)

update u world = 
    case u of
        Viewport vp -> updateViewport vp world
        Frame dt cursor ->
            let fps = Debug.watch "fps" <| floor (1000/dt)
            in Debug.watch "world" <| glide world dt cursor

glide world dt cursor =
    let delta which base =
            let val = which <| addPair (-world.view.w//2,-world.view.h//2) cursor
            in base +  if | val <= -5 && val > -50 -> -1
                          | val >= 5 && val < 50 -> 1
                          | val <= -50 -> -2
                          | val >= 50 -> 2
                          | otherwise -> 0
        pos' = {x=delta fst world.pos.x,y=-(delta snd -world.pos.y)}
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

frames = Signal.map2 Frame frame Mouse.position
dimensions = Signal.map Viewport (Window.dimensions)

inputs = Signal.mergeMany [dimensions,frames]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states