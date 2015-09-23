module Zero where

import Graphics.Collage exposing (..)
import Color exposing (..)

import Signal
import Window
import Signal.Extra

import Time exposing (fps)


-- Model

start u =
    case u of
        Viewport (w,h) -> {width=w,height=h}

-- Update

update u world =
  case u of
    Viewport (w,h) -> {world | width <- w, height <- h}
    _ -> world

-- Display

halfCircle radius =
    let full = circle radius
        total = List.length full
        oneHalf = List.drop 1 <| List.take (total//2) full
    in polygon oneHalf

display world =
    let radius = toFloat world.height/3.4
        gray = (rgb 204 204 204)
    in collage world.width world.height [
        filled gray <| halfCircle radius,
        outlined {defaultLine | width <- 14, cap <- Padded} <| halfCircle radius
    ]

-- Signals

type Update = Frame Float | Viewport (Int,Int)

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        signals = Signal.mergeMany [dimensions,frames]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states