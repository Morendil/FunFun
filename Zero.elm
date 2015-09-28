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
        move (0,gap) <| rotate (degrees 180) <| north line fill radius
    ]

-- Signals

type Update = Frame Float | Viewport (Int,Int)

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        signals = Signal.mergeMany [dimensions,frames]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states