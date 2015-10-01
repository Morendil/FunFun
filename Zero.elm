module Zero where

import Graphics.Collage exposing (..)
import Color exposing (..)
import Easing exposing (..)

import Signal
import Window
import Signal.Extra

import Time exposing (fps)


-- Model

start u =
    case u of
        Viewport (w,h) -> {width=w,height=h,time=0}

-- Update

update u world =
  case u of
    Viewport (w,h) -> {world | width <- w, height <- h}
    Frame dt -> {world | time <- world.time + dt}
    _ -> world

-- Display

halfCircle radius =
    let full = circle radius
        total = List.length full
        oneHalf = List.drop 1 <| List.take (total//2) full
    in polygon oneHalf

north outerColor innerColor radius =
    group [filled innerColor <| halfCircle radius,
           outlined {defaultLine | width <- 14, cap <- Padded, color <- outerColor} <| halfCircle radius]

star time radius =
    let pointy = group [move (radius/1.5,0) <| filled white <| List.map (\(x,y) -> (x,y/2)) <| ngon 3 radius]
        rotated part n = rotate (degrees (n*90)) <| part
        sprite = group <| (filled white <| square (radius/2)) :: List.map (rotated pointy) [0..3]
    in rotate (degrees time/10) sprite

display world =
    let radius = toFloat world.height/3.4
        gap = radius / 14
        gray = (rgb 204 204 204)
        color = ease Easing.linear Easing.color white black 5000 world.time
        line = ease Easing.linear Easing.color black white 5000 world.time
        fill = ease Easing.linear Easing.color gray black 5000 world.time
    in collage world.width world.height [
        filled color <| rect (toFloat world.width) (toFloat world.height),
        move (0,-gap) <| north line fill radius,
        move (0,gap) <| rotate (degrees 180) <| north line fill radius,
        move (gap*5,-gap*5) <| star world.time (radius/15)
    ]

-- Signals

type Update = Frame Float | Viewport (Int,Int)

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        signals = Signal.mergeMany [dimensions,frames]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states