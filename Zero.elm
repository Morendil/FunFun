module Zero where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Color exposing (..)
import Easing exposing (..)

import Signal
import Window
import Signal.Extra

import Time exposing (fps)


-- Model

start u =
    case u of
        Viewport (w,h) -> {width=w,height=h,time=0,items=[
            Background {color=Still white},
            NorthHalf,
            SouthHalf]
        }

type Animated a = Still a | Transition a a Float
type Item = Background {color:Animated Color} | NorthHalf | SouthHalf | Star

-- Update

update u world =
  case u of
    Viewport (w,h) -> {world | width <- w, height <- h}
    Frame dt -> {world | time <- world.time + dt}
    Action South -> {world | items <- List.append world.items [Star]}
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
        line = ease Easing.linear Easing.color black white 5000 world.time
        fill = ease Easing.linear Easing.color gray black 5000 world.time
        place x = collage world.width world.height [x]
        active x = clickable (Signal.message clicks.address x)
        draw item = case item of
            Background {color} ->
                let (Still bg) = color in place <| filled bg <| rect (toFloat world.width) (toFloat world.height)
            NorthHalf -> place <| move (0,-gap) <| north line fill radius
            SouthHalf -> active South <| place <| move (0,gap) <| rotate (degrees 180) <| north line fill radius
            Star -> place <| move (gap*5,-gap*5) <| star world.time (radius/15)
    in layers <| List.map draw world.items

-- Signals

clicks = Signal.mailbox South
type Choice = South | North
type Update = Frame Float | Viewport (Int,Int) | Action Choice

main =
    let frames = Signal.map Frame (fps 30)
        dimensions = Signal.map Viewport (Window.dimensions)
        actions = Signal.map Action clicks.signal
        signals = Signal.mergeMany [dimensions,frames,actions]
        states = Signal.Extra.foldp' update start signals
    in Signal.map display states