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

display world =
    let radius = toFloat world.height/3.4
        gray = (rgb 204 204 204)
    in collage world.width world.height [
        filled black <| circle radius,
        filled gray <| circle (radius-14),
        filled white <| rect (2+radius*2) 14,
        move (0,14) <| filled black <| rect (radius*2-2) 14,
        move (0,-14) <| filled black <| rect (radius*2-2) 14
    ]

-- Signals

type Update = Frame Float | Viewport (Int,Int)

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        signals = Signal.mergeMany [dimensions,frames]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states